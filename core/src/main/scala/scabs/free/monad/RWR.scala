package scabs
package free
package monad

import scabs.Util.{Monad, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeMonad}
import scabs.seq.Sequence

import scala.annotation.tailrec

sealed trait RWR[S[_], F[_], A] {

  final def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G], S: Sequence[S]): G[A] = {
    val result: G[Any] =
      G.tailRecM[RWR[S, F, Any], Any](this.asInstanceOf[RWR[S, F, Any]]) {
        case RWR.Pure(a) =>
          G.pure(Right[RWR[S, F, Any], Any](a.asInstanceOf[Any]))
        case RWR.Seq(nv, s) =>
          G.fmap(trans(nv))(b => Left[RWR[S, F, Any], Any](RWR.run(b, s)))
      }
    result.asInstanceOf[G[A]]
  }

  def cons[B](f: A => RWR[S, F, B])(implicit S: Sequence[S]): RWR[S, F, B]

}

object RWR {

  @tailrec
  final def run[S[_], F[_]](v: Any, nexts: S[Any => RWR[S, F, Any]])(implicit S: Sequence[S]): RWR[S, F, Any] = S.uncons(nexts) match {
    case None => RWR.Pure(v)
    case Some((head, tail)) =>
      head(v) match {
        case RWR.Pure(a) => run(a, tail)
        case RWR.Seq(nv, s) => RWR.Seq[S, F, Any](nv, S.concat(s, tail))
      }
  }

  type Curried[S[_], F[_]] = {type l[A] = RWR[S, F, A]}

  implicit def rwrFreeMonad[S[_], F[_]](implicit S: Sequence[S]): FreeMonad[F, Curried[S, F]#l] =
    new FreeConstraint1[Monad, F, Curried[S, F]#l] {
      override val generated: Monad[Curried[S, F]#l] = new Monad[Curried[S, F]#l] {
        override def pure[A](a: A): RWR[S, F, A] = RWR.Pure(a)

        override def fmap[A, B](fa: RWR[S, F, A])(f: (A) => B): RWR[S, F, B] =
          bind(fa)(f.andThen(RWR.Pure[S, F, B]))

        override def bind[A, B](fa: RWR[S, F, A])(f: (A) => RWR[S, F, B]): RWR[S, F, B] =
          fa.cons(f)

        override def join[A](ffa: RWR[S, F, RWR[S, F, A]]): RWR[S, F, A] =
          bind(ffa)(identity)

        override def tailRecM[A, B](a: A)(f: (A) => RWR[S, F, Either[A, B]]): RWR[S, F, B] =
          RWR.Seq(pure(a), S.cons(f.asInstanceOf[Any => RWR[S, F, Any]],
            S.one(((_: Either[A, B]).fold(tailRecM(_)(f), pure)).asInstanceOf[Any => RWR[S, F, Any]])
          ))
      }

      override def foldMap[A, G[_]](fv: RWR[S, F, A])(trans: ~>[F, G])(implicit ev: Monad[G]): G[A] = ???

      override def retract[A](fv: RWR[S, F, A])(implicit ev: Monad[F]): F[A] = ???
    }

  case class Pure[S[_], F[_], A](a: A) extends RWR[S, F, A] {
    override def cons[B](f: A => RWR[S, F, B])(implicit S: Sequence[S]): RWR[S, F, B] =
      RWR.Seq[S, F, B](this.asInstanceOf[RWR[S, F, Any]], S.one(f.asInstanceOf[Any => RWR[S, F, Any]]))
  }

  case class Lift[S[_], F[_], A](fa: F[A]) extends RWR[S, F, A] {
    override def cons[B](f: A => RWR[S, F, B])(implicit S: Sequence[S]): RWR[S, F, B] =
      RWR.Seq[S, F, B](this.asInstanceOf[RWR[S, F, Any]], S.one(f.asInstanceOf[Any => RWR[S, F, Any]]))
  }

  case class Seq[S[_], F[_], A](value: RWR[S, F, Any], continuations: S[Any => RWR[S, F, Any]]) extends RWR[S, F, A] {
    override def cons[B](f: A => RWR[S, F, B])(implicit S: Sequence[S]): RWR[S, F, B] =
      RWR.Seq[S, F, B](value, S.snoc(continuations, f.asInstanceOf[Any => RWR[S, F, Any]]))
  }

}

