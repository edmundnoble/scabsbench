package scabs
package free
package functor

import scabs.Util.{Functor, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeFunctor}

final case class ADT[F[_], I, A](fi: F[I], f: I => A) {
  def map[B](ab: A => B): ADT[F, I, B] = ADT(fi, ab compose f)
  def foldMap[G[_]](trans: F ~> G)(implicit G: Functor[G]): G[A] = {
    G.fmap(trans(fi))(f)
  }
  def retract(implicit F: Functor[F]): F[A] = {
    F.fmap(fi)(f)
  }
}

object ADT {
  type Curried[F[_]] = {type l[A] = ADT[F, _, A]}

  implicit def freeFunctorADT[F[_]]: FreeFunctor[F, Curried[F]#l] = new FreeConstraint1[Functor, F, Curried[F]#l] {
    val generated: Functor[Curried[F]#l] = new Functor[Curried[F]#l] {
      def fmap[A, B](fa: ADT[F, _, A])(f: (A) => B): ADT[F, _, B] =
        fa.map(f)
    }

    def foldMap[A, G[_]](fv: ADT[F, _, A])(trans: F ~> G)(implicit ev: Functor[G]): G[A] =
      fv.foldMap(trans)

    def retract[A](fv: ADT[F, _, A])(implicit ev: Functor[F]): F[A] =
      fv.retract

    def lift[A](a: F[A]): ADT[F, _, A] =
      ADT(a, identity[A])
  }

}
