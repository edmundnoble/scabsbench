package scabs.free.ap

import scabs.Util.{Applicative, ~>}

sealed trait FreeApADT[F[_], A] {
  def foldMap[G[_] : Applicative](trans: F ~> G): G[A]
}
object FreeApADT {
  case class Pure[F[_], A](value: A) extends FreeApADT[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[A] = G.pure(value)
  }
  case class Ap[F[_], A, B](fa: FreeApADT[F, A], ff: FreeApADT[F, A => B]) extends FreeApADT[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[B] =
      G.ap[A, B](fa.foldMap(trans))(ff.foldMap(trans))
  }
}
