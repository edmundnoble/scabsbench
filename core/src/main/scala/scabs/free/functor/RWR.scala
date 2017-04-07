package scabs
package free
package functor

import scabs.Util.{Functor, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeFunctor}
import scabs.seq.Sequence

final case class RWR[S[_], F[_], I, A](fi: F[I], funs: S[Any => Any]) {
  def map[B](ab: A => B)(implicit S: Sequence[S]): RWR[S, F, I, B] =
    RWR(fi, S.snoc(funs, ab.asInstanceOf[Any => Any]))

  def foldMap[G[_]](trans: F ~> G)(implicit G: Functor[G], S: Sequence[S]): G[A] = {
    val result: G[Any] = G.fmap(trans(fi))(Util.seqRecurse[S](_, funs))
    result.asInstanceOf[G[A]]
  }

  def retract(implicit F: Functor[F], S: Sequence[S]): F[A] = {
    val result: F[Any] = F.fmap(fi)(Util.seqRecurse[S](_, funs))
    result.asInstanceOf[F[A]]
  }
}

object RWR {
  type Curried[S[_], F[_]] = {type l[A] = RWR[S, F, _, A]}

  implicit def freeFunctorRWR[S[_], F[_]](implicit S: Sequence[S]): FreeFunctor[F, Curried[S, F]#l] = new FreeConstraint1[Functor, F, Curried[S, F]#l] {
    override val generated: Functor[Curried[S, F]#l] = new Functor[Curried[S, F]#l] {
      override def fmap[A, B](fa: RWR[S, F, _, A])(f: (A) => B): RWR[S, F, _, B] =
        fa.map(f)
    }

    override def foldMap[A, G[_]](fv: RWR[S, F, _, A])(trans: F ~> G)(implicit ev: Functor[G]): G[A] =
      fv.foldMap(trans)

    override def retract[A](fv: RWR[S, F, _, A])(implicit ev: Functor[F]): F[A] =
      fv.retract

  }

}
