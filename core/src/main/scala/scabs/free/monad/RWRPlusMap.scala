package scabs
package free
package monad

import scabs.Util._
import scabs.free.Constraint.{FreeConstraint1, FreeMonad}
import scabs.seq.{Sequence, TASequence}
import scabs.seq.StdlibInstances._
import cats.syntax.either._

import scala.annotation.tailrec

sealed trait RWRPlusMap[S[_], F[_], A] {

  def flatMap[B](f: A => RWRPlusMap[S, F, B])(implicit S: Sequence[S]): RWRPlusMap[S, F, B]

  def map[B](f: A => B)(implicit S: Sequence[S]): RWRPlusMap[S, F, B]
}

object RWRPlusMap {

  def pure[S[_], F[_], A](value: A): RWRPlusMap[S, F, A] =
    RWRPlusMap.Pure[S, F, A](value)

  final def foldMap[S[_], F[_], G[_], A](rwr: RWRPlusMap[S, F, A])(trans: F ~> G)(implicit G: Monad[G], S: Sequence[S]): G[A] = {
    val result: G[Any] =
      G.tailRecM[RWRPlusMap[S, F, Any], Any](rwr.asInstanceOf[RWRPlusMap[S, F, Any]]) {
        case RWRPlusMap.Pure(a) =>
          G.pure(Right[RWRPlusMap[S, F, Any], Any](a.asInstanceOf[Any]))
        case RWRPlusMap.Seq(nv, s) =>
          G.fmap(trans(nv))(b => Left[RWRPlusMap[S, F, Any], Any](RWRPlusMap.runSeq(b, s)))
        case RWRPlusMap.Map(nv, maps) =>
          G.fmap(Evaluable.tailRecEval(trans(nv), maps))(v => Right(v))
      }
    result.asInstanceOf[G[A]]
  }

  final def retract[S[_], F[_], A](rwr: RWRPlusMap[S, F, A])(implicit F: Monad[F], S: Sequence[S]): F[A] = {
    val result: F[Any] =
      F.tailRecM[RWRPlusMap[S, F, Any], Any](rwr.asInstanceOf[RWRPlusMap[S, F, Any]]) {
        case RWRPlusMap.Pure(a) =>
          F.pure(Right[RWRPlusMap[S, F, Any], Any](a.asInstanceOf[Any]))
        case RWRPlusMap.Seq(nv, s) =>
          F.fmap(nv)(b => Left[RWRPlusMap[S, F, Any], Any](RWRPlusMap.runSeq(b, s)))
        case RWRPlusMap.Map(nv, maps) =>
          Evaluable.tailRecEval(nv, maps.andThen((x: Any) => Either.right[RWRPlusMap[S, F, Any], Any](x)))(S, functionEvaluableMonad[F])
      }
    result.asInstanceOf[F[A]]
  }

//  @tailrec
  final def runSeq[S[_], F[_], A, B](v: A, nexts: TASequence[S, KleisliC[Curried[S, F]#l]#l, A, B])(implicit S: Sequence[S]): RWRPlusMap[S, F, Any] =
    nexts.uncons match {
      case None => pure(v)
      case Some((head, tail)) =>
        val tailAny = tail.asInstanceOf[TASequence[S, KleisliC[Curried[S, F]#l]#l, Any, Any]]
        head(v.asInstanceOf[Nothing]) match {
          case RWRPlusMap.Pure(a) => runSeq(a, tailAny)
          case RWRPlusMap.Seq(nv, s) => RWRPlusMap.Seq[S, F, Any, Any](nv,
            (s ++ tailAny).asInstanceOf[TASequence[S, KleisliC[Curried[S, F]#l]#l, Any, Any]])
          // suspicious. map.flatmap.map.flatmap... stack-safety test?
          case RWRPlusMap.Map(nv, s) =>
            RWRPlusMap.Seq[S, F, Any, Any](nv,
              s.asInstanceOf[TASequence[S, Function1, Any, Any]].retract.andThen(RWRPlusMap.Pure[S, F, Any]) +: tailAny)
        }
    }

  type Curried[S[_], F[_]] = {type l[A] = RWRPlusMap[S, F, A]}

  implicit def rwrPlusMapFreeMonad[S[_], F[_]](implicit S: Sequence[S]): FreeMonad[F, Curried[S, F]#l] =
    new FreeConstraint1[Monad, F, Curried[S, F]#l] {
      override val generated: Monad[Curried[S, F]#l] = new Monad[Curried[S, F]#l] {
        override def pure[A](a: A): RWRPlusMap[S, F, A] =
          RWRPlusMap.pure(a)

        override def fmap[A, B](fa: RWRPlusMap[S, F, A])(f: (A) => B): RWRPlusMap[S, F, B] =
          fa.map(f)

        override def bind[A, B](fa: RWRPlusMap[S, F, A])(f: (A) => RWRPlusMap[S, F, B]): RWRPlusMap[S, F, B] =
          fa.flatMap(f)

        override def join[A](ffa: RWRPlusMap[S, F, RWRPlusMap[S, F, A]]): RWRPlusMap[S, F, A] =
          ffa.flatMap(identity)

        override def tailRecM[A, B](a: A)(f: (A) => RWRPlusMap[S, F, Either[A, B]]): RWRPlusMap[S, F, B] =
          bind(f(a)) {
            case Right(b) => pure(b)
            case Left(next) => tailRecM(next)(f)
          }
      }

      override def foldMap[A, G[_]](fv: RWRPlusMap[S, F, A])(trans: ~>[F, G])(implicit ev: Monad[G]): G[A] =
        RWRPlusMap.foldMap(fv)(trans)

      override def retract[A](fv: RWRPlusMap[S, F, A])(implicit ev: Monad[F]): F[A] =
        RWRPlusMap.retract(fv)
    }

  final case class Pure[S[_], F[_], A](a: A) extends RWRPlusMap[S, F, A] {
    override def flatMap[B](f: A => RWRPlusMap[S, F, B])(implicit S: Sequence[S]): RWRPlusMap[S, F, B] =
      f(a)

    override def map[B](f: A => B)(implicit S: Sequence[S]): RWRPlusMap[S, F, B] =
      Pure(f(a))
  }

  final case class Seq[S[_], F[_], A, B](value: F[A], continuations: TASequence[S, KleisliC[Curried[S, F]#l]#l, A, B]) extends RWRPlusMap[S, F, B] {
    override def flatMap[C](f: B => RWRPlusMap[S, F, C])(implicit S: Sequence[S]): RWRPlusMap[S, F, C] =
      RWRPlusMap.Seq(value, continuations.andThen(f))

    override def map[C](f: B => C)(implicit S: Sequence[S]): RWRPlusMap[S, F, C] =
      RWRPlusMap.Seq(value, continuations.andThen(f.andThen(RWRPlusMap.Pure[S, F, C])))
  }

  final case class Map[S[_], F[_], A, B](value: F[A], maps: TASequence[S, Function1, A, B]) extends RWRPlusMap[S, F, B] {
    override def flatMap[C](f: B => RWRPlusMap[S, F, C])(implicit S: Sequence[S]): RWRPlusMap[S, F, C] =
      RWRPlusMap.Seq[S, F, A, C](value,
        maps.retract.andThen(RWRPlusMap.Pure[S, F, B](_)) +: TASequence.one[S, KleisliC[Curried[S, F]#l]#l, B, C](f))

    override def map[C](f: B => C)(implicit S: Sequence[S]): RWRPlusMap[S, F, C] =
      RWRPlusMap.Map(value, maps.andThen(f))
  }

}

