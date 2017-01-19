package scabs.free.functor

import scabs.Util.{Functor, ~>}
import scabs.seq.Sequence
import scabs.free.Util

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
