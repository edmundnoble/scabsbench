package scabs.free

import scabs.Util._
import scabs.colls.{CSequence, Sequence}
import scabs.colls.Hierarchy._

import scala.annotation.tailrec

sealed trait FreeMonadADT[F[_], A] {
  def foldMap[G[_] : Monad](trans: F ~> G): G[A]
}
object FreeMonadADT {
  case class Pure[F[_], A](value: A) extends FreeMonadADT[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.pure(value)
  }
  case class Roll[F[_], A](roll: F[FreeMonadADT[F, A]]) extends FreeMonadADT[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.bind(trans(roll))(_.foldMap[G](trans))
  }
}

sealed trait FreeMonadADTCPS[F[_], A] {
  def foldMap[G[_] : Monad](trans: F ~> G): G[A]
}
object FreeMonadADTCPS {
  case class Pure[F[_], A](value: A) extends FreeMonadADTCPS[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.pure(value)
  }
  case class Roll[F[_], A](roll: F[A]) extends FreeMonadADTCPS[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      trans(roll)
  }
  case class Bind[F[_], A, B](free: FreeMonadADTCPS[F, A], bind: A => F[B]) extends FreeMonadADTCPS[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[B] =
      G.bind(free.foldMap(trans))(a => trans(bind(a)))
  }
}

sealed trait FreeMonadReflect[S[_], F[_], A] {
  @tailrec
  final def run(v: Any, nexts: S[Any => FreeMonadReflect[S, F, Any]])(implicit S: CSequence[S]): FreeMonadReflect[S, F, Any] = S.sequence.uncons(nexts) match {
    case None => FreeMonadReflect.Pure(v)
    case Some((head, tail)) =>
      head(v) match {
        case FreeMonadReflect.Pure(a) => run(a, tail)
        case FreeMonadReflect.Seq(nv, s) => FreeMonadReflect.Seq[S, F, Any](nv, S.concat(s, tail))
      }
  }

  def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G], S: CSequence[S]): G[A] = {
    val result: G[Any] =
      G.tailRecM[FreeMonadReflect[S, F, Any], Any](this.asInstanceOf[FreeMonadReflect[S, F, Any]]) {
        case FreeMonadReflect.Pure(a) =>
          G.pure(Right[FreeMonadReflect[S, F, Any], Any](a.asInstanceOf[Any]))
        case FreeMonadReflect.Seq(nv, s) =>
          G.fmap(trans(nv))(b => Left[FreeMonadReflect[S, F, Any], Any](run(b, s)))
      }
    result.asInstanceOf[G[A]]
  }
}

object FreeMonadReflect {
  case class Pure[S[_], F[_], A](a: A) extends FreeMonadReflect[S, F, A]
  case class Seq[S[_], F[_], A](value: F[A], continuations: S[Any => FreeMonadReflect[S, F, Any]]) extends FreeMonadReflect[S, F, A]
}


sealed trait FreeMonadReflectFuseMap[S[_], F[_], A] {
  @tailrec
  final def run(v: Any, nexts: S[Any => FreeMonadReflectFuseMap[S, F, Any]])(implicit S: CSequence[S]): FreeMonadReflectFuseMap[S, F, Any] =
    S.sequence.uncons(nexts) match {
    case None => FreeMonadReflectFuseMap.Pure(v)
    case Some((head, tail)) =>
      head(v) match {
        case FreeMonadReflectFuseMap.Pure(a) => run(a, tail)
        case FreeMonadReflectFuseMap.Seq(nv, s) => FreeMonadReflectFuseMap.Seq[S, F, Any](nv, S.concat(s, tail))
        case FreeMonadReflectFuseMap.Map(nv, s) =>
          FreeMonadReflectFuseMap.Seq[S, F, Any](nv,
            S.sequence.cons((a: Any) => FreeMonadReflectFuseMap.Pure(Util.seqRecurse(a, s)), tail))
      }
  }

  def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G], S: CSequence[S]): G[A] = {
    val result: G[Any] =
      G.tailRecM[FreeMonadReflectFuseMap[S, F, Any], Any](this.asInstanceOf[FreeMonadReflectFuseMap[S, F, Any]]) {
        case FreeMonadReflectFuseMap.Pure(a) =>
          G.pure(Right[FreeMonadReflectFuseMap[S, F, Any], Any](a.asInstanceOf[Any]))
        case FreeMonadReflectFuseMap.Seq(nv, s) =>
          G.fmap(trans(nv))(b => Left[FreeMonadReflectFuseMap[S, F, Any], Any](run(b, s)))
        case FreeMonadReflectFuseMap.Map(nv, maps) =>
          G.fmap(trans(nv))(b => Right[FreeMonadReflectFuseMap[S, F, Any], Any](Util.seqRecurse[S](b, maps)))
      }
    result.asInstanceOf[G[A]]
  }
}

object FreeMonadReflectFuseMap {
  case class Pure[S[_], F[_], A](a: A) extends FreeMonadReflectFuseMap[S, F, A]
  case class Seq[S[_], F[_], A](value: F[A], continuations: S[Any => FreeMonadReflectFuseMap[S, F, Any]]) extends FreeMonadReflectFuseMap[S, F, A]
  case class Map[S[_], F[_], A](value: F[A], maps: S[Any => Any]) extends FreeMonadReflectFuseMap[S, F, A]
}

