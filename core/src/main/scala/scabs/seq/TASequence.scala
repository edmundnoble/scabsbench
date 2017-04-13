package scabs
package seq

import scabs.Util._
import scabs.free.Constraint.FreeConstraint2

final case class TASequence[S[_], F[_, _], -A, +B](seq: S[F[Any, Any]]) extends AnyVal {

  def isEmpty(implicit S: Sequence[S]): Boolean = S.isEmpty(seq)

  def length(implicit S: Sequence[S]): Int = S.lengthSeq(seq)

  def transform[G[_, _]](trans: F ~~> G)(implicit S: Sequence[S]): TASequence[S, G, A, B] =
    TASequence[S, G, A, B](S.map(seq)(trans(_)))

  def andThen[BB >: B, C](next: F[BB, C])(implicit S: Sequence[S]): TASequence[S, F, A, C] =
    TASequence[S, F, A, C](S.snoc(seq, next.asInstanceOf[F[Any, Any]]))

  def compose[AA <: A, Z](before: F[Z, AA])(implicit S: Sequence[S]): TASequence[S, F, Z, B] =
    TASequence[S, F, Z, B](S.cons(before.asInstanceOf[F[Any, Any]], seq))

  def ++[C](next: TASequence[S, F, B, C])(implicit S: Sequence[S]): TASequence[S, F, A, C] =
    TASequence[S, F, A, C](S.concat(seq, next.seq))

  def zipWith[BB >: B, C, D](next: TASequence[S, F, Unit, C], zipper: (BB, C) => D)(implicit S: Sequence[S], ev1: Null <:< BB, ev2: Function1 ~~> F): TASequence[S, F, A, D] = {
    var mahB: BB = ev1(null)
    this.++(next.compose(ev2{(b: BB) => mahB = b; ()}).andThen(ev2((c: C) => zipper(mahB, c))))
  }

  def :+[BB >: B, C](next: F[BB, C])(implicit S: Sequence[S]): TASequence[S, F, A, C] =
    andThen(next)

  def +:[AA <: A, Z](before: F[Z, AA])(implicit S: Sequence[S]): TASequence[S, F, Z, B] =
    compose(before)

  def foldMap[AA <: A, BB >: B, G[_, _]](trans: F ~~> G)(implicit S: Sequence[S], G: CategoryTailrec[G]): G[AA, BB] =
    G.tailRecP(transform(trans))

  def retract[AA <: A, BB >: B](implicit S: Sequence[S], F: CategoryTailrec[F]): F[AA, BB] =
    F.tailRecP(this)

  def foreach(trans: F ~~> BiConst[Unit]#l)(implicit S: Sequence[S]): Unit =
    S.foreach(seq)(trans(_))

  def uncons[AA <: A](implicit S: Sequence[S]): Option[(F[AA, Any], TASequence[S, F, Any, B])] =
    S.uncons(seq).map(t => t._1.asInstanceOf[F[AA, Any]] -> TASequence[S, F, Any, B](t._2))

}

object TASequence {

  type Curried[S[_], F[_, _]] = {type l[A, B] = TASequence[S, F, A, B]}

  implicit def taSequenceFreeCategoryTailrec[R[_], F[_, _]](implicit R: Sequence[R]): FreeConstraint2[CategoryTailrec, F, Curried[R, F]#l] =
    new FreeConstraint2[CategoryTailrec, F, Curried[R, F]#l] {
      override val generated: CategoryTailrec[Curried[R, F]#l] = new CategoryTailrec[Curried[R, F]#l] {
        override def id[A]: TASequence[R, F, A, A] =
          TASequence.empty[R, F, A]

        override def compose[A, B, C](ab: TASequence[R, F, A, B], bc: TASequence[R, F, B, C]): TASequence[R, F, A, C] =
          ab ++ bc

//        override def tailRecP[R[_], A, B](seq: TASequence[R, Curried[S, F]#l, A, B])(implicit R: Sequence[R]): TASequence[R, F, A, B] =
//          TASequence[R, F, A, B](S.rebuild[R, F[Any, Any]](seq.retract(R, generated).seq))
        override def tailRecP[S[_] : Sequence, A, B](seq: TASequence[S, Curried[R, F]#l, A, B]): TASequence[R, F, A, B] = ???
      }

      override def foldMap[A, B, G[_, _]](fv: TASequence[R, F, A, B])(trans: ~~>[F, G])(implicit G: CategoryTailrec[G]): G[A, B] =
        fv.foldMap(trans)

      override def retract[A, B](fv: TASequence[R, F, A, B])(implicit F: CategoryTailrec[F]): F[A, B] =
        fv.retract
      override def lift[A, B](a: F[A, B]): TASequence[R, F, A, B] =
        TASequence.one(a)
    }

  def empty[S[_], F[_, _], A](implicit S: Sequence[S]): TASequence[S, F, A, A] =
    TASequence[S, F, A, A](S.empty)

  def one[S[_], F[_, _], A, B](f: F[A, B])(implicit S: Sequence[S]): TASequence[S, F, A, B] =
    TASequence[S, F, A, B](S.one(f.asInstanceOf[F[Any, Any]]))

}
