package scabs.free

import scabs.Util._

object Constraint {

  trait FreeConstraint0[F[_], A] {
    def foldMap[B](trans: A => B)(implicit ev: F[B]): B

    def retract(implicit ev: F[A]): A
  }

  trait FreeConstraint1[S[_[_]], F[_], A] {
    def foldMap[G[_]](trans: F ~> G)(implicit ev: S[G]): G[A]

    def retract(implicit ev: S[F]): F[A]
  }

  type FreeMonoid[A] = FreeConstraint0[Monoid, A]

  type FreeFunctor[F[_], A] = FreeConstraint1[Functor, F, A]

  object FreeFunctor {
    def inj[F[_], A](fa: F[A]): FreeFunctor[F, A] = new FreeConstraint1[Functor, F, A] {
      override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Functor[G]): G[A] = trans(fa)

      override def retract(implicit ev: Functor[F]): F[A] = fa
    }

    def map[F[_], A, B](frf: FreeFunctor[F, A], f: A => B): FreeFunctor[F, B] = new FreeConstraint1[Functor, F, B] {
      override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Functor[G]): G[B] = ev.fmap(frf.foldMap(trans))(f)

      override def retract(implicit ev: Functor[F]): F[B] = ev.fmap(frf.retract)(f)
    }
  }

  type FreeApplicative[F[_], A] = FreeConstraint1[Applicative, F, A]

  object FreeApplicative {
    def map2[F[_], A, B, C](fa: FreeApplicative[F, A], fb: FreeApplicative[F, B])(f: (A, B) => C): FreeApplicative[F, C] =
      new FreeConstraint1[Applicative, F, C] {
        override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[C] =
          ev.map2(fa.foldMap(trans), fb.foldMap(trans))(f)

        override def retract(implicit ev: Applicative[F]): F[C] =
          ev.map2(fa.retract, fb.retract)(f)
      }

    def ap[F[_], A, B](fa: FreeApplicative[F, A])(ff: FreeApplicative[F, A => B]): FreeApplicative[F, B] = new FreeConstraint1[Applicative, F, B] {
      override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] =
        ev.ap(fa.foldMap(trans))(ff.foldMap(trans))

      override def retract(implicit ev: Applicative[F]): F[B] =
        ev.ap(fa.retract)(ff.retract)
    }

    def inj[F[_], A](fa: F[A]): FreeApplicative[F, A] = new FreeConstraint1[Applicative, F, A] {
      override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[A] = trans(fa)

      override def retract(implicit ev: Applicative[F]): F[A] = fa
    }

    def fmap[F[_], A, B](frf: FreeApplicative[F, A])(f: A => B): FreeApplicative[F, B] = new FreeConstraint1[Applicative, F, B] {
      override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] = ev.fmap(frf.foldMap(trans))(f)

      override def retract(implicit ev: Applicative[F]): F[B] = ev.fmap(frf.retract)(f)
    }
  }

  type FreeMonad[F[_], A] = FreeConstraint1[Monad, F, A]
}
