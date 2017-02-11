package scabs
package seq

import scabs.Util.Monad

import scala.annotation.tailrec

sealed trait LTree[A] {
  def size: Int
}
case class Lf[A](a: A) extends LTree[A] {
  override def size: Int = 1
}
case class Bin[A](override val size: Int, left: LTree[A], right: LTree[A]) extends LTree[A]

object LTree {
  def join[A](left: LTree[A], right: LTree[A]) = Bin(left.size + right.size, left, right)
  def frontier[A](tree: LTree[A]): List[A] = {
    def helper(innerTree: LTree[A]): List[A] => List[A] = innerTree match {
      case Lf(a) => a :: _
      case Bin(_, left, right) => helper(left) compose helper(right)
    }
    helper(tree)(Nil)
  }
  @tailrec
  def index[A](idx: Int, lTree: LTree[A]): A = lTree match {
    case Lf(a) => if (idx == 0) a else ???
    case Bin(size, left, right) =>
      if (idx < left.size) index(idx, left)
      else index(idx - left.size, right)
  }
  def update[A](idx: Int, newValue: A, lTree: LTree[A]): LTree[A] = lTree match {
    case tr@Lf(a) => if (idx == 0) Lf(newValue) else tr
    case Bin(size, left, right) =>
      if (idx < left.size) Bin(size, update(idx, newValue, left), right)
      else Bin(size, left, update(idx - left.size, newValue, right))
  }

  def delLast[A](lTree: LTree[A]): LTree[A] = lTree match {
    case Bin(_, left, Lf(_)) => left
    case Bin(n, l, r) => Bin(n - 1, l, delLast(r))
  }

  def delFirst[A](lTree: LTree[A]): LTree[A] = lTree match {
    case Bin(_, Lf(_), right) => right
    case Bin(n, l, r) => Bin(n - 1, delFirst(l), r)
  }

  def forestOfComp[A](list: List[A]): List[LTree[A]] = {
    def insComp(t: LTree[A], ts: List[LTree[A]]): List[LTree[A]] = {
      if (ts.isEmpty) t :: Nil
      else if (t.size == ts.head.size) insComp(join(t, ts.head), ts)
      else t :: ts
    }
    list.map(Lf(_)).foldRight(Nil: List[LTree[A]]) (insComp)
  }

  implicit val ltreeMonad: Monad[LTree] = new Monad[LTree] {
    override def pure[A](a: A): LTree[A] = Lf(a)

    override def bind[A, B](fa: LTree[A])(f: (A) => LTree[B]): LTree[B] = fa match {
      case Lf(a) => f(a)
      case Bin(_, l, r) => LTree.join(bind(l)(f), bind(r)(f))
    }

    override def fmap[A, B](fa: LTree[A])(f: (A) => B): LTree[B] = ???

    override def join[A](ffa: LTree[LTree[A]]): LTree[A] = ???

    override def tailRecM[A, B](a: A)(f: (A) => LTree[Either[A, B]]): LTree[B] = ???
  }

}
