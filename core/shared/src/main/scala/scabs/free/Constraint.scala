package scabs.free

import scabs.Util._

trait FreeConstraint0[F[_], A] {
  def foldMap[B](trans: A => B)(implicit ev: F[B]): B
}

trait FreeConstraint1[S[_[_]], F[_], A] {
  def foldMap[G[_]](trans: F ~> G)(implicit ev: S[G]): G[A]
}

object Constraint {
  type FreeMonoid0[A] = FreeConstraint0[Monoid, A]
  def empty[A]: FreeMonoid0[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = B.mempty
  }
  def inj[A](value: A): FreeMonoid0[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = trans(value)
  }
  def append[A](fst: FreeMonoid0[A], snd: FreeMonoid0[A]): FreeMonoid0[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B =
      B.mappend(trans(fst.foldMap(trans)), trans(snd.foldMap(trans)))
  }
  type FreeFunctor1[F[_], A] = FreeConstraint1[Functor, F, A]
  def inj[F[_], A](fa: F[A]): FreeFunctor1[F, A] = new FreeConstraint1[Functor, F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Functor[G]): G[A] = trans(fa)
  }
  def map[F[_], A, B](frf: FreeFunctor1[F, A], f: A => B): FreeFunctor1[F, B] = new FreeConstraint1[Functor, F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Functor[G]): G[B] = ev.fmap(frf.foldMap(trans))(f)
  }
  type FreeMonadJoin1[F[_], A] = FreeConstraint1[MonadJoin, F, A]
  object FreeMonadJoin1 {
    def pure[F[_], A](a: A): FreeMonadJoin1[F, A] = new FreeConstraint1[MonadJoin, F, A] {
      override def foldMap[G[_]](trans: F ~> G)(implicit G: MonadJoin[G]): G[A] = G.pure(a)
    }
    def ofAlgebra[F[_], A](alg: F[A]): FreeMonadJoin1[F, A] = new FreeConstraint1[MonadJoin, F, A] {
      override def foldMap[G[_]](trans: F ~> G)(implicit G: MonadJoin[G]): G[A] = trans(alg)
    }
    def flatMap[F[_], A, B](fa: FreeMonadJoin1[F, A], f: A => FreeMonadJoin1[F, B]): FreeMonadJoin1[F, B] = new FreeMonadJoin1[F, B] {
      override def foldMap[G[_]](trans: F ~> G)(implicit G: MonadJoin[G]): G[B] =
        G.join(G.fmap(fa.foldMap[G](trans))(f(_).foldMap(trans)))
    }
  }
  type FreeMonadBind1[F[_], A] = FreeConstraint1[MonadBind, F, A]
}
