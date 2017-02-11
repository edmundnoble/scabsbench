package scabs
package free
package ap

import scabs.Util.{Applicative, Traverse, ~>}
import scabs.free.Constraint.{FreeApplicative, FreeConstraint1}

sealed trait ADT[F[_], A] {
  def foldMap[G[_] : Applicative](trans: F ~> G): G[A]
  def retract(implicit F: Applicative[F]): F[A]
}
object ADT {
  type Curried[F[_]] = {type l[A] = ADT[F, A]}
  implicit def freeApplicativeADT[F[_]]: FreeApplicative[F, Curried[F]#l] = new FreeConstraint1[Applicative, F, Curried[F]#l] {
    override implicit val generated: Applicative[Curried[F]#l] = new Applicative[Curried[F]#l] {
      override def pure[A](a: A): ADT[F, A] =
        Pure(a)

      override def ap[A, B](fa: ADT[F, A])(ff: ADT[F, A => B]): ADT[F, B] =
        Ap(fa, ff)

      override def traverse[S[_] : Traverse, A, B](fa: S[A])(f: (A) => ADT[F, B]): ADT[F, S[B]] =
        Traverse[S].traverse[Curried[F]#l, A, B](fa)(f)

      override def sequence[S[_] : Traverse, A](fa: S[ADT[F, A]]): ADT[F, S[A]] =
        Traverse[S].sequence[Curried[F]#l, A](fa)
    }

    override def foldMap[A, G[_]](fv: ADT[F, A])(trans: F ~> G)(implicit ev: Applicative[G]): G[A] = fv.foldMap(trans)

    override def retract[A](fv: ADT[F, A])(implicit ev: Applicative[F]): F[A] = fv.retract

  }

  case class Pure[F[_], A](value: A) extends ADT[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[A] =
      G.pure(value)

    override def retract(implicit F: Applicative[F]): F[A] =
      F.pure(value)
  }

  case class Ap[F[_], A, B](fa: ADT[F, A], ff: ADT[F, A => B]) extends ADT[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Applicative[G]): G[B] =
      G.ap(fa.foldMap(trans))(ff.foldMap(trans))

    override def retract(implicit F: Applicative[F]): F[B] =
      F.ap(fa.retract)(ff.retract)
  }
}
