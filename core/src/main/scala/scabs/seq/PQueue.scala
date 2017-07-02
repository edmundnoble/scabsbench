package scabs
package seq

// todo: these data structures expect lists to be non-empty but I've not bothered encoding this
// each non-empty list could be modelled by pulling the head directly into the case class, and having
// a possibly empty tail, but life is short

sealed trait PQueue[A] {
  def :+(a: A): PQueue[A]

  def +:(a: A): PQueue[A]

  def headTail: Option[(A, PQueue[A])]

  def initsLast: Option[(PQueue[A], A)]

  def fold[B](z: B)(f: (B, A) => B): B

  def isEmpty: Boolean

  def length: Int

  def ++(other: PQueue[A]): PQueue[A]
}

// todo: Either cheat with a single instance of this that gets cast, or use variance +A and an object
final case class PNil[A]() extends PQueue[A] {
  override def :+(a: A): PQueue[A] = PTails(a :: Nil) // could choose Heads

  override def +:(a: A): PQueue[A] = PHeads(a :: Nil) // could choose Tails

  override def headTail: Option[(A, PQueue[A])] = None

  override def initsLast: Option[(PQueue[A], A)] = None

  override def isEmpty: Boolean = true

  override def ++(other: PQueue[A]): PQueue[A] = other

  override def length: Int = 0

  override def fold[B](z: B)(f: (B, A) => B): B = z
}

final case class PHeads[A](heads: List[A]) extends PQueue[A] {
  override def :+(a: A): PQueue[A] = PQ[A](heads, a :: Nil)

  override def +:(a: A): PQueue[A] = PHeads(a :: heads)

  override def headTail: Option[(A, PQueue[A])] = heads match {
    case Nil => None
    case h :: Nil =>
      Some((h, PNil()))
    case h :: t =>
      Some((h, PHeads(t)))
  }

  override def initsLast: Option[(PQueue[A], A)] =
    PTails(heads.reverse).initsLast

  override def isEmpty: Boolean = false

  override def ++(other: PQueue[A]): PQueue[A] = other match {
    case PTails(tails) =>
      PQ(heads, tails)
    case PAppend(PTails(tails), right) =>
      PAppend(PQ(heads, tails), right)
    case PNil() =>
      this
    case _ =>
      PAppend(this, other)
  }

  override def length: Int = heads.length

  override def fold[B](z: B)(f: (B, A) => B): B = heads.foldLeft(z)(f)
}

final case class PTails[A](tails: List[A]) extends PQueue[A] {
  override def :+(a: A): PQueue[A] = PTails(a :: tails)

  override def +:(a: A): PQueue[A] = PQ(a :: Nil, tails)

  override def headTail: Option[(A, PQueue[A])] = PHeads(tails.reverse).headTail

  override def initsLast: Option[(PQueue[A], A)] = tails match {
    case Nil => None
    case l :: Nil =>
      Some((PNil(), l))
    case l :: i =>
      Some((PTails(i), l))
  }

  override def isEmpty: Boolean = false

  override def ++(other: PQueue[A]): PQueue[A] = other match {
    case PHeads(heads) =>
      PQ(heads, tails)
    case PNil() =>
      this
    case _ =>
      PAppend(this, other)
  }

  override def length: Int = tails.length

  override def fold[B](z: B)(f: (B, A) => B): B = tails.reverse.foldLeft(z)(f)
}


final case class PQ[A](heads: List[A], tails: List[A]) extends PQueue[A] {
  override def :+(a: A): PQueue[A] = PQ(heads, a :: tails)

  override def +:(a: A): PQueue[A] = PQ(a :: heads, tails)

  override def headTail: Option[(A, PQueue[A])] = heads match {
    case Nil => None
    case h :: Nil =>
      Some((h, PTails(tails)))
    case h :: t =>
      Some((h, PQ(t, tails)))
  }

  override def initsLast: Option[(PQueue[A], A)] = tails match {
    case Nil => None
    case l :: Nil =>
      Some((PHeads(heads), l))
    case l :: i =>
      Some((PQ(heads, i), l))
  }

  override def isEmpty: Boolean = false

  override def ++(other: PQueue[A]): PQueue[A] = other match {
    case PNil() =>
      this
    case _ =>
      PAppend(this, other)
  }

  override def length: Int = heads.length + tails.length

  override def fold[B](z: B)(f: (B, A) => B): B =
    tails.reverse.foldLeft(heads.foldLeft(z)(f))(f)
}

final case class PAppend[A](left: PQueue[A], right: PQueue[A]) extends PQueue[A] {
  override def :+(a: A): PQueue[A] = PAppend(left, right :+ a)

  override def +:(a: A): PQueue[A] = PAppend(a +: left, right)

  override def headTail: Option[(A, PQueue[A])] = left.headTail.map {
    case ((h, leftT)) =>
      if (leftT.isEmpty)
        (h, right)
      else
        (h, PAppend(leftT, right))
  }

  override def initsLast: Option[(PQueue[A], A)] = right.initsLast.map {
    case ((rightI, l)) =>
      if (rightI.isEmpty)
        (left, l)
      else
        (PAppend(left, rightI), l)
  }

  override def isEmpty: Boolean = false

  override def ++(other: PQueue[A]): PQueue[A] = (left, right, other) match {
    case (_, _, PNil()) =>
      this
    case (_, PHeads(heads), PTails(tails)) =>
      PAppend(left, PQ(heads, tails))
    case _ =>
      PAppend(this, other)
  }

  override def length: Int = left.length + right.length

  override def fold[B](z: B)(f: (B, A) => B): B = headTail match {
    case Some((h, t)) => t.fold(f(z, h))(f)
    case None => z
  }
}

object PQueue {

  implicit val sequenceForPQueue: Sequence[PQueue] = new Sequence[PQueue] {
    override def empty[A]: PQueue[A] = PNil()

    override def isEmpty[A](q: PQueue[A]): Boolean = q.isEmpty

    override def head[A](queue: PQueue[A]): A = queue.headTail.get._1

    override def last[A](queue: PQueue[A]): A = queue.initsLast.get._2

    override def init[A](queue: PQueue[A]): PQueue[A] = queue.initsLast.get._1

    override def tail[A](queue: PQueue[A]): PQueue[A] = queue.headTail.get._2

    override def cons[A](x: A, q: PQueue[A]): PQueue[A] = x +: q

    override def snoc[A](q: PQueue[A], y: A): PQueue[A] = q :+ y

    override def lengthSeq[A](q: PQueue[A]): Int = q.length

    override def cata[A, B](q: PQueue[A])(z: B)(f: (B, A) => B): B = q.fold(z)(f)

    override def toList[A](q: PQueue[A]): List[A] = q.fold[List[A]](Nil)((a, v) => v :: a)

    override def toSeq[A](xs: List[A]): PQueue[A] = xs.foldLeft(empty[A])((a, v) => v +: a)

    override def uncons[A](s: PQueue[A]): Option[(A, PQueue[A])] = s.headTail

    override def unsnoc[A](s: PQueue[A]): Option[(PQueue[A], A)] = s.initsLast

    override def map[A, B](q: PQueue[A])(f: (A) => B): PQueue[B] = q.fold(empty[B])((s, x) => f(x) +: s)

    override def foreach[A, U](q: PQueue[A])(f: (A) => U): Unit = map(q)(f)

    override def concat[A](fst: PQueue[A], snd: PQueue[A]): PQueue[A] = fst ++ snd
  }

}
