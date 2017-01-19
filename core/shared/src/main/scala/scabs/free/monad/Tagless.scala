package scabs.free.monad

import scabs.Util.{Monad, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeMonad}

object Tagless {
  def pure[F[_], A](a: A): FreeMonad[F, A] = new FreeConstraint1[Monad, F, A] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] = G.pure(a)
    override def retract(implicit ev: Monad[F]): F[A] = ev.pure(a)
  }
  def lift[F[_], A](alg: F[A]): FreeMonad[F, A] = new FreeConstraint1[Monad, F, A] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] = trans(alg)
    override def retract(implicit ev: Monad[F]): F[A] = alg
  }
  def bind[F[_], A, B](fa: FreeMonad[F, A])(f: A => FreeMonad[F, B]): FreeMonad[F, B] = new FreeConstraint1[Monad, F, B] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[B] =
      G.bind(fa.foldMap[G](trans))(f(_).foldMap(trans))
    override def retract(implicit F: Monad[F]): F[B] = F.bind(fa.retract)(f(_).retract)
  }
}
