package scabs
package free
package ap

import scabs.Util.{Applicative, ~>}
import shapeless.ops.hlist._
import shapeless._
import scabs.seq.Sequence

object ApSequence {

  def empty[S[_], F[_]](implicit S: Sequence[S]): ApSequence[S, F, HNil] =
    ApSequence[S, F, HNil](S.empty)

}

final case class ApSequence[S[_], F[_], U <: HList](seq: S[F[Any]]) extends AnyVal {

  def retract[H, T <: HList](implicit S: Sequence[S], F: Applicative[F]): F[U] = S.uncons(seq) match {
    case None => F.pure(HNil.asInstanceOf[U])
    case Some((h, t)) => F.map2(h, ApSequence[S, F, HList](t).retract)(_ :: _).asInstanceOf[F[U]]
  }

  def foldMap[H, G[_], T <: HList](trans: F ~> G)(implicit S: Sequence[S], G: Applicative[G]): G[U] = S.uncons(seq) match {
    case None => G.pure(HNil.asInstanceOf[U])
    case Some((h, t)) => G.map2(trans(h), ApSequence[S, F, HList](t).foldMap(trans))(_ :: _).asInstanceOf[G[U]]
  }

  def isEmpty(implicit S: Sequence[S]): Boolean = S.isEmpty(seq)

  def length(implicit S: Sequence[S]): Int = S.lengthSeq(seq)

  def transform[G[_]](trans: F ~> G)(implicit S: Sequence[S]): ApSequence[S, G, U] =
    ApSequence[S, G, U](S.map(seq)(trans(_)))

  def andThen[B](next: F[B])(implicit S: Sequence[S]): ApSequence[S, F, B :: U] =
    ApSequence[S, F, B :: U](S.snoc(seq, next.asInstanceOf[F[Any]]))

  def ++[V <: HList, UV <: HList](next: ApSequence[S, F, V])(implicit S: Sequence[S], au: Prepend.Aux[U, V, UV]): ApSequence[S, F, UV] =
    ApSequence[S, F, UV](S.concat(seq, next.seq))

  def :+[B](next: F[B])(implicit S: Sequence[S]): ApSequence[S, F, B :: U] =
    andThen(next)

}

