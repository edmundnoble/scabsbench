package scabs
package free
package ap

import cats._
import cats.implicits._
import scabs.free.Constraint.{FreeApplicative, FreeConstraint1}

sealed trait ADT[F[_], A] {
  def foldMap[G[_] : Applicative](trans: F ~> G): G[A]

  def retract(implicit F: Applicative[F]): F[A]
}
object ADT {
  type Curried[F[_]] = {type l[A] = ADT[F, A]}
  implicit def freeApplicativeADT[F[_]]: FreeApplicative[F, Curried[F]#l] =
    new FreeConstraint1[Applicative, F, Curried[F]#l] {
      implicit val generated: Applicative[Curried[F]#l] = new Applicative[Curried[F]#l] {
        def pure[A](a: A): ADT[F, A] =
          Pure(a)

        def ap[A, B](ff: ADT[F, A => B])(fa: ADT[F, A]): ADT[F, B] =
          Ap(fa, ff)

        def traverse[S[_] : Traverse, A, B](fa: S[A])(f: (A) => ADT[F, B]): ADT[F, S[B]] =
          Traverse[S].traverse[Curried[F]#l, A, B](fa)(f)

        override def sequence[S[_] : Traverse, A](fa: S[ADT[F, A]]): ADT[F, S[A]] =
          Traverse[S].sequence[Curried[F]#l, A](fa)
      }

      def foldMap[A, G[_]](fv: ADT[F, A])(trans: F ~> G)(implicit ev: Applicative[G]): G[A] = fv.foldMap(trans)

      def retract[A](fv: ADT[F, A])(implicit ev: Applicative[F]): F[A] = fv.retract

      def lift[A](a: F[A]): ADT[F, A] = Lift(a)
    }

  case class Pure[F[_], A](value: A) extends ADT[F, A] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[A] =
      G.pure(value)

    def retract(implicit F: Applicative[F]): F[A] =
      F.pure(value)
  }

  case class Lift[F[_], A](value: F[A]) extends ADT[F, A] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[A] =
      trans(value)

    def retract(implicit F: Applicative[F]): F[A] =
      value
  }

  case class Ap[F[_], A, B](fa: ADT[F, A], ff: ADT[F, A => B]) extends ADT[F, B] {
    def foldMap[G[_]](trans: F ~> G)(implicit G: Applicative[G]): G[B] =
      G.ap(ff.foldMap(trans))(fa.foldMap(trans))

    def retract(implicit F: Applicative[F]): F[B] =
      F.ap(ff.retract)(fa.retract)
  }
}
