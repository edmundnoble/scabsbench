package scabs

import simulacrum.typeclass

object Util {

  type Const[A, B] = A
  type Id[A] = A
  type ~>[F[_], G[_]] = FunctionK[F, G]
  type <~[F[_], G[_]] = FunctionK[G, F]
  type <~~>[F[_], G[_]] = (F ~> G, G ~> F)
  type Kleisli[F[_], A, B] = A => F[B]
  type Cokleisli[F[_], A, B] = F[A] => B
  type Algebra[F[_], A] = A => F[A]
  type Colgebra[F[_], A] = F[A] => A

  trait FunctionK[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  @typeclass trait Monoid[A] {
    def mempty: A
    def mappend(fst: A, snd: A): A
  }

  @typeclass trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
    def tailRecF[A, B](fa: F[A])(f: A => A Either B): F[B]
  }

  @typeclass trait Applicative[F[_]] {
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  @typeclass trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    def tailRecM[A, B](a: A)(f: A => F[A Either B]): F[B]
    def join[A](ffa: F[F[A]]): F[A]
  }

}
