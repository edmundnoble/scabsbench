package scabs
package free
package functor

import scabs.Util.{Functor, ~>}

final case class ADT[F[_], I, A](fi: F[I], f: I => A) {
  def map[B](ab: A => B): ADT[F, I, B] = ADT(fi, ab compose f)
  def foldMap[G[_]](trans: F ~> G)(implicit G: Functor[G]): G[A] = {
    G.fmap(trans(fi))(f)
  }
  def retract(implicit F: Functor[F]): F[A] = {
    F.fmap(fi)(f)
  }
}
