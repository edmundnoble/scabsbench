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
    override val generated: Functor[Curried[F]#l] = new Functor[Curried[F]#l] {
      override def fmap[A, B](fa: ADT[F, _, A])(f: (A) => B): ADT[F, _, B] =
        fa.map(f)

      override def tailRecF[A, B](fa: ADT[F, _, A])(f: (A) => Either[A, B]): ADT[F, _, B] = {
        def loop(a: A): B = f(a).fold(loop, identity)
        fmap(fa)(loop)
      }
    }

    override def foldMap[A, G[_]](fv: ADT[F, _, A])(trans: F ~> G)(implicit ev: Functor[G]): G[A] =
      fv.foldMap(trans)

    override def retract[A](fv: ADT[F, _, A])(implicit ev: Functor[F]): F[A] =
      fv.retract

  }

}
