package scabs.free

import scabs.Util._

trait FreeConstraint0[F[_], A] {
  def foldMap[B](trans: A => B)(implicit ev: F[B]): B

  def retract(implicit ev: F[A]): A
}

trait FreeConstraint1[S[_[_]], F[_], A] {
  def foldMap[G[_]](trans: F ~> G)(implicit ev: S[G]): G[A]

  def retract(implicit ev: S[F]): F[A]
}

object Constraint {
  type FreeMonoid0[A] = FreeConstraint0[Monoid, A]
  def empty[A]: FreeMonoid0[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = B.mempty
    override def retract(implicit ev: Monoid[A]): A = ev.mempty
  }
  def inj[A](value: A): FreeMonoid0[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = trans(value)
    override def retract(implicit ev: Monoid[A]): A = value
  }
  def append[A](fst: FreeMonoid0[A], snd: FreeMonoid0[A]): FreeMonoid0[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B =
      B.mappend(fst.foldMap(trans), snd.foldMap(trans))
    override def retract(implicit ev: Monoid[A]): A = ev.mappend(fst.retract, snd.retract)
  }
  type FreeFunctor1[F[_], A] = FreeConstraint1[Functor, F, A]
  def inj[F[_], A](fa: F[A]): FreeFunctor1[F, A] = new FreeConstraint1[Functor, F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Functor[G]): G[A] = trans(fa)
    override def retract(implicit ev: Functor[F]): F[A] = fa
  }
  def map[F[_], A, B](frf: FreeFunctor1[F, A], f: A => B): FreeFunctor1[F, B] = new FreeConstraint1[Functor, F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Functor[G]): G[B] = ev.fmap(frf.foldMap(trans))(f)
    override def retract(implicit ev: Functor[F]): F[B] = ev.fmap(frf.retract)(f)
  }
  type FreeApplicative1[F[_], A] = FreeConstraint1[Applicative, F, A]
  def map2[F[_], A, B, C](fa: FreeApplicative1[F, A], fb: FreeApplicative1[F, B])(f: (A, B) => C): FreeApplicative1[F, C] =
    new FreeConstraint1[Applicative, F, C] {
      override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[C] =
        ev.map2(trans(fa.foldMap(trans)), trans(fb.foldMap(trans)))(f)
      override def retract(implicit ev: Applicative[F]): F[C] =
        ev.map2(fa.retract, fb.retract)(f)
    }
  def ap[F[_], A, B](fa: FreeApplicative1[F, A])(ff: FreeApplicative1[F, A => B]): FreeApplicative1[F, B] = new FreeConstraint1[Applicative, F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] =
      ev.ap(trans(fa.foldMap(trans)))(trans(ff.foldMap(trans)))
    override def retract(implicit ev: Applicative[F]): F[B] =
      ev.ap(fa.retract)(ff.retract)
  }
  def inj[F[_], A](fa: F[A]): FreeApplicative1[F, A] = new FreeConstraint1[Applicative, F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[A] = trans(fa)
    override def retract(implicit ev: Applicative[F]): F[A] = fa
  }
  def fmap[F[_], A, B](frf: FreeApplicative1[F, A])(f: A => B): FreeApplicative1[F, B] = new FreeConstraint1[Applicative, F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] = ev.fmap(frf.foldMap(trans))(f)
    override def retract(implicit ev: Applicative[F]): F[B] = ev.fmap(frf.retract)(f)
  }
  type FreeMonad1[F[_], A] = FreeConstraint1[Monad, F, A]
  object FreeMonad1 {
    def pure[F[_], A](a: A): FreeMonad1[F, A] = new FreeConstraint1[Monad, F, A] {
      override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] = G.pure(a)
      override def retract(implicit ev: Monad[F]): F[A] = ev.pure(a)
    }
    def ofAlgebra[F[_], A](alg: F[A]): FreeMonad1[F, A] = new FreeConstraint1[Monad, F, A] {
      override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] = trans(alg)
      override def retract(implicit ev: Monad[F]): F[A] = alg
    }
    def bind[F[_], A, B](fa: FreeMonad1[F, A])(f: A => FreeMonad1[F, B]): FreeMonad1[F, B] = new FreeConstraint1[Monad, F, B] {
      override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[B] =
        G.bind(fa.foldMap[G](trans))(f(_).foldMap(trans))
      override def retract(implicit F: Monad[F]): F[B] = F.bind(fa.retract)(f(_).retract)
    }
  }
}
