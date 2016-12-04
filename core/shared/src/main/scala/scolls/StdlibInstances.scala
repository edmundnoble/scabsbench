package scolls

import scala.collection.immutable.{Queue, Stack}

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

  implicit val queueSequenceInstance: Sequence[Queue] = new Sequence[Queue] {
    override def empty[A]: Queue[A] = Queue.empty[A]
    override def isEmpty[A](q: Queue[A]): Boolean = q.isEmpty
    override def head[A](queue: Queue[A]): A = queue.head
    override def last[A](queue: Queue[A]): A = queue.last
    override def init[A](queue: Queue[A]): Queue[A] = queue.init
    override def tail[A](queue: Queue[A]): Queue[A] = queue.tail
    override def cons[A](x: A, q: Queue[A]): Queue[A] = x +: q
    override def snoc[A](q: Queue[A], y: A): Queue[A] = q :+ y
    override def lengthSeq[A](q: Queue[A]): Int = q.length
    override def toList[A](q: Queue[A]): List[A] = q.toList
    override def toSeq[A](xs: List[A]): Queue[A] = xs.to[Queue]
    override def fold[A, B](q: Queue[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)
  }

  implicit val queueCSequenceInstance: CSequence[Queue] = new CSequence[Queue] {
    override def sequence: Sequence[Queue] = queueSequenceInstance
    override def concat[A](fst: Queue[A], snd: Queue[A]): Queue[A] = fst ++ snd
  }

  implicit val streamSequenceInstance: Sequence[Stream] = new Sequence[Stream] {
    override def empty[A]: Stream[A] = Stream.empty[A]
    override def isEmpty[A](q: Stream[A]): Boolean = q.isEmpty
    override def head[A](stream: Stream[A]): A = stream.head
    override def last[A](stream: Stream[A]): A = stream.last
    override def init[A](stream: Stream[A]): Stream[A] = stream.init
    override def tail[A](stream: Stream[A]): Stream[A] = stream.tail
    override def cons[A](x: A, q: Stream[A]): Stream[A] = x +: q
    override def snoc[A](q: Stream[A], y: A): Stream[A] = q :+ y
    override def lengthSeq[A](q: Stream[A]): Int = q.length
    override def toList[A](q: Stream[A]): List[A] = q.toList
    override def toSeq[A](xs: List[A]): Stream[A] = xs.to[Stream]
    override def fold[A, B](q: Stream[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)
  }

  implicit val streamCSequenceInstance: CSequence[Stream] = new CSequence[Stream] {
    override def sequence: Sequence[Stream] = streamSequenceInstance
    override def concat[A](fst: Stream[A], snd: Stream[A]): Stream[A] = fst ++ snd
  }

}
