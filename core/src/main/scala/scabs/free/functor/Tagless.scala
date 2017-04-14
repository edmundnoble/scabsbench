package scabs
package free
package functor

import cats._
import cats.implicits._
import scabs.free.Constraint.{FreeConstraint1, FreeFunctor}

sealed abstract class Tagless[F[_], A] {
  def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[A]
  def retract(implicit F: Functor[F]): F[A]
}

object Tagless {
  type Curried[F[_]] = {type l[A] = Tagless[F, A]}
  def map[F[_], A, B](self: Tagless[F, A])(fun: A => B): Tagless[F, B] = new Tagless[F, B] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[B] =
      G.map(self.foldMap(trans))(fun)
    def retract(implicit F: Functor[F]): F[B] =
      F.map(self.retract)(fun)
  }
  def lift[F[_], A](value: F[A]): Tagless[F, A] = new Tagless[F, A] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[A] =
      trans(value)
    def retract(implicit F: Functor[F]): F[A] =
      value
  }

  implicit def freeFunctorTagless[F[_]]: FreeFunctor[F, Curried[F]#l] = new FreeConstraint1[Functor, F, Curried[F]#l] {
    val generated: Functor[Curried[F]#l] = new Functor[Curried[F]#l] {
      def map[A, B](fa: Tagless[F, A])(f: (A) => B): Tagless[F, B] =
        Tagless.map(fa)(f)
    }

    def foldMap[A, G[_]](fv: Tagless[F, A])(trans: F ~> G)(implicit ev: Functor[G]): G[A] =
      fv.foldMap(trans)

    def retract[A](fv: Tagless[F, A])(implicit ev: Functor[F]): F[A] =
      fv.retract

    def lift[A](a: F[A]): Tagless[F, A] =
      Tagless.lift(a)
  }
}

