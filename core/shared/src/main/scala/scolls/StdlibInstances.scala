package scolls

object StdlibInstances {

  implicit val vectorSequenceInstance: Sequence[Vector] = new Sequence[Vector] {
    override def empty[A]: Vector[A] = Vector.empty[A]
    override def isEmpty[A](q: Vector[A]): Boolean = q.isEmpty
    override def head[A](queue: Vector[A]): A = queue.head
    override def last[A](queue: Vector[A]): A = queue.last
    override def init[A](queue: Vector[A]): Vector[A] = queue.init
    override def tail[A](queue: Vector[A]): Vector[A] = queue.tail
    override def cons[A](x: A, q: Vector[A]): Vector[A] = x +: q
    override def snoc[A](q: Vector[A], y: A): Vector[A] = q :+ y
    override def lengthSeq[A](q: Vector[A]): Int = q.length
    override def toList[A](q: Vector[A]): List[A] = q.toList
    override def toSeq[A](xs: List[A]): Vector[A] = xs.toVector
    override def fold[A, B](q: Vector[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)
  }

  implicit val vectorCSequenceInstance: CSequence[Vector] = new CSequence[Vector] {
    override def sequence: Sequence[Vector] = vectorSequenceInstance
    override def concat[A](fst: Vector[A], snd: Vector[A]): Vector[A] = fst ++ snd
  }

  implicit val listSequenceInstance: Sequence[List] = new Sequence[List] {
    override def empty[A]: List[A] = List.empty[A]
    override def isEmpty[A](q: List[A]): Boolean = q.isEmpty
    override def head[A](queue: List[A]): A = queue.head
    override def last[A](queue: List[A]): A = queue.last
    override def init[A](queue: List[A]): List[A] = queue.init
    override def tail[A](queue: List[A]): List[A] = queue.tail
    override def cons[A](x: A, q: List[A]): List[A] = x +: q
    override def snoc[A](q: List[A], y: A): List[A] = q :+ y
    override def lengthSeq[A](q: List[A]): Int = q.length
    override def toList[A](q: List[A]): List[A] = q
    override def toSeq[A](xs: List[A]): List[A] = xs
    override def fold[A, B](q: List[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)
  }

  implicit val listCSequenceInstance: CSequence[List] = new CSequence[List] {
    override def sequence: Sequence[List] = listSequenceInstance
    override def concat[A](fst: List[A], snd: List[A]): List[A] = fst ++ snd
  }

}
