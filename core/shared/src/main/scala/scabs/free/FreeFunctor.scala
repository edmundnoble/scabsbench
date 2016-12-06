package scabs.free

import scabs.Util.{Functor, ~>}
import scabs.colls.Sequence

import scala.annotation.tailrec

final case class FreeFunctorADT[F[_], I, A](fi: F[I], f: I => A) {
  def map[B](ab: A => B): FreeFunctorADT[F, I, B] = FreeFunctorADT(fi, ab compose f)
  def foldMap[G[_]](trans: F ~> G)(implicit G: Functor[G]): G[A] = {
    G.fmap(trans(fi))(f)
  }
}

final case class FreeFunctorSeq[S[_], F[_], I, A](fi: F[I], funs: S[Any => Any]) {
  def map[B](ab: A => B)(implicit S: Sequence[S]): FreeFunctorSeq[S, F, I, B] =
    FreeFunctorSeq(fi, S.snoc(funs, ab.asInstanceOf[Any => Any]))

  @tailrec
  def go(v: Any, funs: S[Any => Any])(implicit S: Sequence[S]): Any = {
    S.uncons(funs) match {
      case Some((head, tail)) =>
        go(head(v), tail)
      case None =>
        v
    }
  }

  def foldMap[G[_]](trans: F ~> G)(implicit G: Functor[G], S: Sequence[S]): G[A] = {
    val result: G[Any] = G.fmap(trans(fi))(go(_, funs))
    result.asInstanceOf[G[A]]
  }

  def retract(implicit F: Functor[F], S: Sequence[S]): F[A] = {
    val result: F[Any] = F.fmap(fi)(go(_, funs))
    result.asInstanceOf[F[A]]
  }
}
