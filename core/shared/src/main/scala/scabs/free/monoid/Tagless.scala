package scabs.free.monoid

import scabs.Util.Monoid
import scabs.free.Constraint.{FreeConstraint0, FreeMonoid}

object Tagless {

  def empty[A]: FreeMonoid[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = B.mempty

    override def retract(implicit ev: Monoid[A]): A = ev.mempty
  }

  def inj[A](value: A): FreeMonoid[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = trans(value)

    override def retract(implicit ev: Monoid[A]): A = value
  }

  def append[A](fst: FreeMonoid[A], snd: FreeMonoid[A]): FreeMonoid[A] = new FreeConstraint0[Monoid, A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B =
      B.mappend(fst.foldMap(trans), snd.foldMap(trans))

    override def retract(implicit ev: Monoid[A]): A = ev.mappend(fst.retract, snd.retract)
  }

}
