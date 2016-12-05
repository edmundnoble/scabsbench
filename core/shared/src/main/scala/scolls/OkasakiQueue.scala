package scolls

case class OkasakiQueue[A](left: List[A], right: List[A])

object OkasakiQueue {

  private val emptyQ: OkasakiQueue[Nothing] = OkasakiQueue[Nothing](Nil, Nil)

  implicit val okasakiQueueSequenceInstance = new Sequence[OkasakiQueue] {

    def empty[A]: OkasakiQueue[A] =
      emptyQ.asInstanceOf[OkasakiQueue[A]]

    def isEmpty[A](q: OkasakiQueue[A]): Boolean =
      q.left.isEmpty

    def head[A](queue: OkasakiQueue[A]): A =
      queue.left.head

    def last[A](queue: OkasakiQueue[A]): A =
      if (queue.right.nonEmpty) queue.right.head else queue.left.last

    def tail[A](queue: OkasakiQueue[A]): OkasakiQueue[A] =
      normQ(queue.left.tail, queue.right)

    def init[A](queue: OkasakiQueue[A]): OkasakiQueue[A] =
      if (queue.right.isEmpty) OkasakiQueue(queue.left.init, Nil)
      else OkasakiQueue(queue.left, queue.right.tail)

    def cons[A](x: A, q: OkasakiQueue[A]): OkasakiQueue[A] =
      OkasakiQueue(x :: q.left, q.right)

    def snoc[A](q: OkasakiQueue[A], y: A): OkasakiQueue[A] =
      normQ(q.left, y :: q.right)

    def lengthSeq[A](q: OkasakiQueue[A]): Int =
      q.left.length + q.right.length

    def toList[A](q: OkasakiQueue[A]): List[A] =
      q.left ++ q.right.reverse

    def toSeq[A](xs: List[A]): OkasakiQueue[A] =
      OkasakiQueue(xs, Nil)

    def normQ[A](left: List[A], right: List[A]): OkasakiQueue[A] =
      if (left.isEmpty) OkasakiQueue(right.reverse, Nil) else OkasakiQueue(left, right)

    def fold[A, B](q: OkasakiQueue[A])(z: B)(f: (B, A) => B): B =
      q.right.reverse.foldLeft(q.left.foldLeft(z)(f))(f)

    def uncons[A](s: OkasakiQueue[A]): Option[(A, OkasakiQueue[A])] =
      if (isEmpty(s)) None else Some((head(s), tail(s)))

    def unsnoc[A](s: OkasakiQueue[A]): Option[(OkasakiQueue[A], A)] = {
      if (s.right.nonEmpty) Some((OkasakiQueue[A](s.left, s.right.tail), s.right.head))
      else if (s.left.nonEmpty) StdlibInstances.listSequenceInstance.unsnoc[A](s.left).map { case (l, a) => (OkasakiQueue(l, Nil), a) }
      else None
    }
  }

  implicit val okasakiQueueCSequenceInstance = new CSequence[OkasakiQueue] {
    def concat[A](fst: OkasakiQueue[A], snd: OkasakiQueue[A]): OkasakiQueue[A] =
      OkasakiQueue(fst.left ++ fst.right.reverse ++ snd.left, snd.right)

    override def sequence: Sequence[OkasakiQueue] = okasakiQueueSequenceInstance
  }

}
