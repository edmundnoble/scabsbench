package scabs
package free
package functor

import scabs.Util.{Functor, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeFunctor}

trait Tagless[F[_], A] {
  def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[A]
  def retract(implicit F: Functor[F]): F[A]
}

object Tagless {
  def map[F[_], A, B](self: Tagless[F, A])(fun: A => B): Tagless[F, B] = new Tagless[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[B] =
      G.fmap(self.foldMap(trans))(fun)
    override def retract(implicit F: Functor[F]): F[B] =
      F.fmap(self.retract)(fun)
  }
  def lift[F[_], A](value: F[A]): Tagless[F, A] = new Tagless[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[A] =
      trans(value)
    override def retract(implicit F: Functor[F]): F[A] =
      value
  }
}

