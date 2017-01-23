package scabs.seq

import scabs.Util.{BiConst, Category, CategoryTailrec, ~~>}
import scabs.free.Constraint.{FreeCategory, FreeConstraint2}

final case class TASequence[S[_], F[_, _], A, B](seq: S[F[Any, Any]]) extends AnyVal {

  import TASequence.Curried

  def isEmpty(implicit S: Sequence[S]): Boolean = S.isEmpty(seq)

  def length(implicit S: Sequence[S]): Int = S.lengthSeq(seq)

  def transform[G[_, _]](trans: F ~~> G)(implicit S: Sequence[S]): TASequence[S, G, A, B] =
    TASequence[S, G, A, B](S.map(seq)(trans(_)))

  def andThen[C](next: F[B, C])(implicit S: Sequence[S]): TASequence[S, F, A, C] =
    TASequence[S, F, A, C](S.snoc(seq, next.asInstanceOf[F[Any, Any]]))

  def compose[Z](before: F[Z, A])(implicit S: Sequence[S]): TASequence[S, F, Z, B] =
    TASequence[S, F, Z, B](S.cons(before.asInstanceOf[F[Any, Any]], seq))

  def ++[C](next: TASequence[S, F, B, C])(implicit S: Sequence[S]): TASequence[S, F, A, C] =
    TASequence[S, F, A, C](S.concat(seq, next.seq))

  def :+[C](next: F[B, C])(implicit S: Sequence[S]): TASequence[S, F, A, C] =
    andThen(next)

  def +:[Z](before: F[Z, A])(implicit S: Sequence[S]): TASequence[S, F, Z, B] =
    compose(before)

  def foldMap[G[_, _]](trans: F ~~> G)(implicit S: Sequence[S], G: CategoryTailrec[G]): G[A, B] =
    G.tailRecP(transform(trans))

  def retract(implicit S: Sequence[S], F: CategoryTailrec[F]): F[A, B] =
    F.tailRecP(this)

  def foreach(trans: F ~~> BiConst[Unit]#l)(implicit S: Sequence[S]): Unit = {
    S.foreach(seq)(trans(_))
  }

  def uncons(implicit S: Sequence[S]): Option[(F[A, Any], TASequence[S, F, Any, B])] =
    S.uncons(seq).map(t => t._1.asInstanceOf[F[A, Any]] -> TASequence[S, F, Any, B](t._2))

}

object TASequence {

  type Curried[S[_], F[_, _]] = {type l[A, B] = TASequence[S, F, A, B]}

  implicit def taSequenceFreeCategoryTailrec[S[_], F[_, _]](implicit S: Sequence[S]): FreeConstraint2[CategoryTailrec, F, Curried[S, F]#l] =
    new FreeConstraint2[CategoryTailrec, F, Curried[S, F]#l] {
      override val generated: CategoryTailrec[Curried[S, F]#l] = new CategoryTailrec[Curried[S, F]#l] {
        override def id[A]: TASequence[S, F, A, A] =
          TASequence.empty[S, F, A]

        override def compose[A, B, C](ab: TASequence[S, F, A, B], bc: TASequence[S, F, B, C]): TASequence[S, F, A, C] =
          ab ++ bc

        override def tailRecP[R[_], A, B](seq: TASequence[R, Curried[S, F]#l, A, B])(implicit R: Sequence[R]): TASequence[R, F, A, B] =
          TASequence[R, F, A, B](S.rebuild[R, F[Any, Any]](seq.retract(R, generated).seq))
      }

      override def foldMap[A, B, G[_, _]](fv: TASequence[S, F, A, B])(trans: ~~>[F, G])(implicit G: CategoryTailrec[G]): G[A, B] =
        fv.foldMap(trans)

      override def retract[A, B](fv: TASequence[S, F, A, B])(implicit F: CategoryTailrec[F]): F[A, B] =
        fv.retract
    }

  def empty[S[_], F[_, _], A](implicit S: Sequence[S]): TASequence[S, F, A, A] =
    TASequence[S, F, A, A](S.empty)

  def one[S[_], F[_, _], A, B](f: F[A, B])(implicit S: Sequence[S]): TASequence[S, F, A, B] =
    TASequence[S, F, A, B](S.one(f.asInstanceOf[F[Any, Any]]))

}
