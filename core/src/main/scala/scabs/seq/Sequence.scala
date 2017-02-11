package scabs
package seq

import simulacrum.typeclass

@typeclass trait Sequence[S[_]] {

  def empty[A]: S[A]

  def isEmpty[A](q: S[A]): Boolean

  def head[A](queue: S[A]): A

  def last[A](queue: S[A]): A

  def init[A](queue: S[A]): S[A]

  def tail[A](queue: S[A]): S[A]

  def cons[A](x: A, q: S[A]): S[A]

  def snoc[A](q: S[A], y: A): S[A]

  def lengthSeq[A](q: S[A]): Int

  def fold[A, B](q: S[A])(z: B)(f: (B, A) => B): B

  def toList[A](q: S[A]): List[A]

  def toSeq[A](xs: List[A]): S[A]

  def uncons[A](s: S[A]): Option[(A, S[A])]

  def unsnoc[A](s: S[A]): Option[(S[A], A)]

  def map[A, B](q: S[A])(f: A => B): S[B]

  def foreach[A, U](q: S[A])(f: A => U): Unit

  def concat[A](fst: S[A], snd: S[A]): S[A]

  def one[A](a: A): S[A] = cons(a, empty)

  def rebuild[R[_], A](rs: S[A])(implicit R: Sequence[R]): R[A] =
    fold[A, R[A]](rs)(R.empty)(R.snoc)

}

