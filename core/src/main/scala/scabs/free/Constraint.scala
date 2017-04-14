package scabs
package free

import cats.{Applicative, Functor, Monad, Monoid, ~>}
import scabs.Util._

object Constraint {

  trait FreeConstraint0[F[_], A, V] {
    implicit val generated: F[V]

    def lift(a: A): V

    def foldMap[B](v: V)(trans: A => B)(implicit ev: F[B]): B

    def retract(v: V)(implicit ev: F[A]): A
  }

  object FreeConstraint0 {
    implicit def getGenerated[F[_], A, V](implicit frc: FreeConstraint0[F, A, V]): F[V] = frc.generated
  }

  trait FreeConstraint1[S[_[_]], F[_], V[_]] {
    implicit val generated: S[V]

    def lift[A](a: F[A]): V[A]

    def foldMap[A, G[_]](fv: V[A])(trans: F ~> G)(implicit ev: S[G]): G[A]

    def retract[A](fv: V[A])(implicit ev: S[F]): F[A]
  }

  object FreeConstraint1 {
    implicit def getGenerated[S[_[_]], F[_], V[_]](implicit frc: FreeConstraint1[S, F, V]): S[V] = frc.generated
  }

  trait FreeConstraint2[S[_[_, _]], F[_, _], V[_, _]] {
    val generated: S[V]

    def lift[A, B](a: F[A, B]): V[A, B]

    def foldMap[A, B, G[_, _]](fv: V[A, B])(trans: F ~~> G)(implicit ev: S[G]): G[A, B]

    def retract[A, B](fv: V[A, B])(implicit ev: S[F]): F[A, B]
  }

  object FreeConstraint2 {
    implicit def getGenerated[S[_[_, _]], F[_, _], V[_, _]](implicit frc: FreeConstraint2[S, F, V]): S[V] = frc.generated
  }

  type FreeMonoid[A, V] = FreeConstraint0[Monoid, A, V]

  type FreeFunctor[F[_], V[_]] = FreeConstraint1[Functor, F, V]

  type FreeApplicative[F[_], V[_]] = FreeConstraint1[Applicative, F, V]

  type FreeMonad[F[_], V[_]] = FreeConstraint1[Monad, F, V]

  type FreeCategory[F[_, _], V[_, _]] = FreeConstraint2[Category, F, V]
}
