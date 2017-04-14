package scabs
package free
package monad

import cats._
import cats.implicits._
import scabs.free.Constraint.{FreeConstraint1, FreeMonad}

sealed trait Tagless[F[_], A] {
  def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A]

  def retract(implicit ev: Monad[F]): F[A]
}

object Tagless {
  type Curried[F[_]] = {type l[A] = Tagless[F, A]}

  implicit def taglessFreeMonad[F[_]]: FreeMonad[F, Curried[F]#l] = new FreeConstraint1[Monad, F, Curried[F]#l] {
    val generated: Monad[Curried[F]#l] = new Monad[Curried[F]#l] {
      def pure[A](a: A): Tagless[F, A] =
        Tagless.pure(a)

      override def map[A, B](fa: Tagless[F, A])(f: (A) => B): Tagless[F, B] =
        Tagless.map(fa)(f)

      def flatMap[A, B](fa: Tagless[F, A])(f: (A) => Tagless[F, B]): Tagless[F, B] =
        Tagless.flatMap(fa)(f)

      override def flatten[A](ffa: Tagless[F, Tagless[F, A]]): Tagless[F, A] =
        Tagless.flatten(ffa)

      def tailRecM[A, B](a: A)(f: (A) => Tagless[F, Either[A, B]]): Tagless[F, B] =
        Tagless.flatMap(f(a))(_.fold(tailRecM(_)(f), pure))
    }

    def foldMap[A, G[_]](fv: Tagless[F, A])(trans: ~>[F, G])(implicit ev: Monad[G]): G[A] =
      fv.foldMap(trans)

    def retract[A](fv: Tagless[F, A])(implicit ev: Monad[F]): F[A] =
      fv.retract

    def lift[A](a: F[A]): Tagless[F, A] =
      Tagless.lift(a)
  }

  def pure[F[_], A](a: A): Tagless[F, A] = new Tagless[F, A] {
    def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] =
      G.pure(a)

    def retract(implicit F: Monad[F]): F[A] =
      F.pure(a)
  }

  def map[F[_], A, B](fa: Tagless[F, A])(f: A => B): Tagless[F, B] = new Tagless[F, B] {
    def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[B] =
      G.map(fa.foldMap(trans))(f)

    def retract(implicit F: Monad[F]): F[B] =
      F.map(fa.retract)(f)
  }

  def lift[F[_], A](alg: F[A]): Tagless[F, A] = new Tagless[F, A] {
    def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] =
      trans(alg)

    def retract(implicit ev: Monad[F]): F[A] =
      alg
  }

  def flatMap[F[_], A, B](fa: Tagless[F, A])(f: A => Tagless[F, B]): Tagless[F, B] = new Tagless[F, B] {
    def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[B] =
      G.flatMap(fa.foldMap(trans))(f(_).foldMap(trans))

    def retract(implicit F: Monad[F]): F[B] =
      F.flatMap(fa.retract)(f(_).retract)
  }

  def flatten[F[_], A](ffa: Tagless[F, Tagless[F, A]]): Tagless[F, A] = new Tagless[F, A] {
    def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] =
      G.flatMap(ffa.foldMap(trans))(_.foldMap(trans))

    def retract(implicit F: Monad[F]): F[A] =
      F.flatMap(ffa.retract)(_.retract)
  }

}
