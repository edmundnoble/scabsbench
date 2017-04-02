package scabs
package seq

import CatenableArrLeaves._

/**
  * Trivial catenable sequence. Supports O(1) append, and (amortized)
  * O(1) `uncons`, such that walking the sequence via N successive `uncons`
  * steps takes O(N). Like a difference list, conversion to a `Seq[A]`
  * takes linear time, regardless of how the sequence is built up.
  */
sealed abstract class CatenableArrLeaves[+A] {

  /** Returns the head and tail of this catenable if non empty, none otherwise. Amortized O(1). */
  final def uncons: Option[(A, CatenableArrLeaves[A])] = {
    var c: CatenableArrLeaves[A] = this
    val rights = new collection.mutable.ArrayBuffer[CatenableArrLeaves[A]]
    var result: Option[(A, CatenableArrLeaves[A])] = null
    while (result eq null) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            result = None
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
        case Leaf(a, l) =>
          val next = if (rights.isEmpty) empty else rights.reduceLeft((x, y) => Append(y,x))
          val arrShifted = new Array[AnyRef](4)
          var i = 1
          while (i < l) {
            arrShifted(i - 1) = a(i)
            i += 1
          }
          result = Some(a(0).asInstanceOf[A] -> Append(Leaf(arrShifted, l - 1), next))
        case Append(l, r) => c = l; rights += r
      }
    }
    result
  }

  /** Returns true if there are no elements in this collection. */
  def isEmpty: Boolean

  /** Concatenates this with `c` in O(1) runtime. */
  final def ++[A2>:A](c: CatenableArrLeaves[A2]): CatenableArrLeaves[A2] =
    append(this, c)

  /** Returns a new catenable consisting of `a` followed by this. O(1) runtime. */
  final def cons[A2>:A](a: A2): CatenableArrLeaves[A2] =
    this match {
      case Empty => single(a)
      case Leaf(arr, l) if l < 4 =>
        val newArr = new Array[AnyRef](4)
        var i = 0
        while (i < l) {
          newArr(i) = arr(i)
          i += 1
        }
        newArr(i) = a.asInstanceOf[AnyRef]
        Leaf(newArr, l + 1)
      case _ => append(single(a), this)
    }

  /** Alias for [[cons]]. */
  final def +:[A2>:A](a: A2): CatenableArrLeaves[A2] =
    cons(a)

  /** Returns a new catenable consisting of this followed by `a`. O(1) runtime. */
  final def snoc[A2>:A](a: A2): CatenableArrLeaves[A2] =
    this match {
      case Empty => single(a)
      case Leaf(arr, l) if l < 4 =>
        val newArr = new Array[AnyRef](4)
        var i = 0
        while (i < l) {
          newArr(i + 1) = arr(i)
          i += 1
        }
        newArr(0) = a.asInstanceOf[AnyRef]
        Leaf(newArr, l + 1)
      case _ => append(this, single(a))
    }

  /** Alias for [[snoc]]. */
  final def :+[A2>:A](a: A2): CatenableArrLeaves[A2] =
    snoc(a)

  /** Applies the supplied function to each element and returns a new catenable. */
  final def map[B](f: A => B): CatenableArrLeaves[B] =
    foldLeft(empty: CatenableArrLeaves[B])((acc, a) => acc :+ f(a))

  /** Folds over the elements from left to right using the supplied initial value and function. */
  final def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var result = z
    foreach(a => result = f(result, a))
    result
  }

  /** Applies the supplied function to each element, left to right. */
  final def foreach[U](f: A => U): Unit = {
    var c: CatenableArrLeaves[A] = this
    val rights = new collection.mutable.ArrayBuffer[CatenableArrLeaves[A]]
    while (c ne null) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            c = null
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
        case Leaf(a, l) =>
          var i = 0
          while (i < l) {
            f(a(i).asInstanceOf[A])
            i += 1
          }
          c = if (rights.isEmpty) Empty else rights.reduceLeft((x, y) => Append(y,x))
          rights.clear()
        case Append(l, r) => c = l; rights += r
      }
    }
  }

  /** Converts to a list. */
  final def toList: List[A] = {
    val builder = List.newBuilder[A]
    foreach { a => builder += a; () }
    builder.result
  }

  override def toString = "CatenableArrLeaves(..)"
}

object CatenableArrLeaves {
  private final case object Empty extends CatenableArrLeaves[Nothing] {
    def isEmpty: Boolean = true
  }
  private final case class Leaf[A](arr: Array[AnyRef], length: Int) extends CatenableArrLeaves[A] {
    def isEmpty: Boolean = false
  }
  private final case class Append[A](left: CatenableArrLeaves[A], right: CatenableArrLeaves[A]) extends CatenableArrLeaves[A] {
    def isEmpty: Boolean = false // b/c `append` constructor doesn't allow either branch to be empty
  }

  /** Empty catenable. */
  val empty: CatenableArrLeaves[Nothing] = Empty

  /** Creates a catenable of 1 element. */
  def single[A](a: A): CatenableArrLeaves[A] = {
    val arr = new Array[AnyRef](4)
    arr(0) = a.asInstanceOf[AnyRef]
    Leaf(arr, length = 1)
  }

  /** Appends two catenables. */
  def append[A](c: CatenableArrLeaves[A], c2: CatenableArrLeaves[A]): CatenableArrLeaves[A] =
    if (c.isEmpty) c2
    else if (c2.isEmpty) c
    else Append (c, c2)

  /** Creates a catenable from the specified sequence. */
  def fromSeq[A](s: Seq[A]): CatenableArrLeaves[A] =
    if (s.isEmpty) empty
    else s.view.reverse.map(single).reduceLeft((x, y) => Append(y, x))

  /** Creates a catenable from the specified elements. */
  def apply[A](as: A*): CatenableArrLeaves[A] = {
    as match {
      case w: collection.mutable.WrappedArray[A] =>
        if (w.isEmpty) empty
        else if (w.size == 1) single(w.head)
        else {
          val arr: Array[A] = w.array
          var c: CatenableArrLeaves[A] = single(arr.last)
          var idx = arr.length - 2
          while (idx >= 0) {
            c = Append(single(arr(idx)), c)
            idx -= 1
          }
          c
        }
      case _ => fromSeq(as)
    }
  }

  implicit val catenableSequenceInstance = new Sequence[CatenableArrLeaves] {
    override def empty[A]: CatenableArrLeaves[A] = CatenableArrLeaves.empty
    override def isEmpty[A](q: CatenableArrLeaves[A]): Boolean = q.isEmpty
    override def head[A](queue: CatenableArrLeaves[A]): A = queue.uncons.get._1
    override def last[A](queue: CatenableArrLeaves[A]): A = queue.toList.last
    override def init[A](queue: CatenableArrLeaves[A]): CatenableArrLeaves[A] = fromSeq(queue.toList.init)
    override def tail[A](queue: CatenableArrLeaves[A]): CatenableArrLeaves[A] = queue.uncons.get._2
    override def cons[A](x: A, q: CatenableArrLeaves[A]): CatenableArrLeaves[A] = x +: q
    override def snoc[A](q: CatenableArrLeaves[A], y: A): CatenableArrLeaves[A] = q :+ y
    override def lengthSeq[A](q: CatenableArrLeaves[A]): Int = q.toList.length
    override def fold[A, B](q: CatenableArrLeaves[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)
    override def toList[A](q: CatenableArrLeaves[A]): List[A] = q.toList
    override def toSeq[A](xs: List[A]): CatenableArrLeaves[A] = fromSeq(xs)
    override def uncons[A](s: CatenableArrLeaves[A]): Option[(A, CatenableArrLeaves[A])] = s.uncons
    override def unsnoc[A](s: CatenableArrLeaves[A]): Option[(CatenableArrLeaves[A], A)] =
      if (s.isEmpty) None else {
        val list = s.toList
        Some((CatenableArrLeaves.fromSeq(list.init), list.last))
      }
    override def map[A, B](q: CatenableArrLeaves[A])(f: (A) => B): CatenableArrLeaves[B] = q.map(f)
    override def concat[A](fst: CatenableArrLeaves[A], snd: CatenableArrLeaves[A]): CatenableArrLeaves[A] = fst ++ snd
    override def foreach[A, U](q: CatenableArrLeaves[A])(f: (A) => U): Unit = q.foreach(f)
  }
}
