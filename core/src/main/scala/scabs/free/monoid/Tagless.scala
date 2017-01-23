package scabs
package free
package monoid

import scabs.Util.Monoid
import scabs.free.Constraint.{FreeConstraint0, FreeMonoid}

object Tagless {

  sealed trait Tagless[A] {
    def foldMap[B: Monoid](trans: A => B): B
    def retract(implicit A: Monoid[A]): A
  }

  implicit def taglessFreeMonoid[A] = new FreeMonoid[A, Tagless[A]] {
    override def foldMap[B](v: Tagless[A])(trans: (A) => B)(implicit ev: Monoid[B]): B = v.foldMap(trans)
    override def retract(v: Tagless[A])(implicit ev: Monoid[A]): A = v.retract

    override val generated: Monoid[Tagless[A]] = new Monoid[Tagless[A]] {
      override def mempty: Tagless[A] = Tagless.empty[A]

      override def mappend(fst: Tagless[A], snd: Tagless[A]): Tagless[A] = Tagless.mappend(fst, snd)
    }
  }

  def empty[A]: Tagless[A] = new Tagless[A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = B.mempty
    override def retract(implicit ev: Monoid[A]): A = ev.mempty
  }

  def inj[A](value: A): Tagless[A] = new Tagless[A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B = trans(value)
    override def retract(implicit ev: Monoid[A]): A = value
  }

  def mappend[A](fst: Tagless[A], snd: Tagless[A]): Tagless[A] = new Tagless[A] {
    override def foldMap[B](trans: (A) => B)(implicit B: Monoid[B]): B =
      B.mappend(fst.foldMap(trans), snd.foldMap(trans))
    override def retract(implicit ev: Monoid[A]): A = ev.mappend(fst.retract, snd.retract)
  }

}
