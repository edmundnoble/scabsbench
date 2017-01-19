package scabs.free.monad

import scabs.Util._
import scabs.seq.Sequence
import scabs.free.Util

import scala.annotation.tailrec

sealed trait ADT[F[_], A] {
  def foldMap[G[_] : Monad](trans: F ~> G): G[A]
}
object ADT {
  case class Pure[F[_], A](value: A) extends ADT[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.pure(value)
  }
  case class Roll[F[_], A](roll: F[ADT[F, A]]) extends ADT[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.bind(trans(roll))(_.foldMap[G](trans))
  }
}

sealed trait ADTCPS[F[_], A] {
  def foldMap[G[_] : Monad](trans: F ~> G): G[A]
}
object ADTCPS {
  case class Pure[F[_], A](value: A) extends ADTCPS[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.pure(value)
  }
  case class Roll[F[_], A](roll: F[A]) extends ADTCPS[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      trans(roll)
  }
  case class Bind[F[_], A, B](free: ADTCPS[F, A], bind: A => F[B]) extends ADTCPS[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[B] =
      G.bind(free.foldMap(trans))(a => trans(bind(a)))
  }
}

