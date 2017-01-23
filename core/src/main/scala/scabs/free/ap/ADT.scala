package scabs
package free
package ap

import scabs.Util.{Applicative, ~>}

sealed trait ADT[F[_], A] {
  def foldMap[G[_] : Applicative](trans: F ~> G): G[A]
}
object ADT {
  case class Pure[F[_], A](value: A) extends ADT[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[A] = G.pure(value)
  }
  case class Ap[F[_], A, B](fa: ADT[F, A], ff: ADT[F, A => B]) extends ADT[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[B] =
      G.ap[A, B](fa.foldMap(trans))(ff.foldMap(trans))
  }
}
