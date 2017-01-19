package scabs.free.functor

import scabs.Util.{Functor, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeFunctor}

object Tagless {
  def map[F[_], A, B](self: FreeFunctor[F, A])(fun: A => B): FreeFunctor[F, B] = new FreeConstraint1[Functor, F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[B] =
      G.fmap(self.foldMap(trans))(fun)
    override def retract(implicit F: Functor[F]): F[B] =
      F.fmap(self.retract)(fun)
  }
  def lift[F[_], A](value: F[A]): FreeFunctor[F, A] = new FreeConstraint1[Functor, F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[A] =
      trans(value)
    override def retract(implicit F: Functor[F]): F[A] =
      value
  }
}

