package scolls

case class HQueue[A](size: Int, left: List[A], right: List[A])

object HQueue {
  implicit val hQueueSequenceInstance: Sequence[HQueue] = new Sequence[HQueue] {
    override def empty[A]: HQueue[A] = HQueue(0, Nil, Nil)
    override def isEmpty[A](q: HQueue[A]): Boolean = q.size == 0
    override def head[A](queue: HQueue[A]): A = if (queue.left.nonEmpty) queue.left.head else queue.right.head
    override def last[A](queue: HQueue[A]): A = if (queue.right.nonEmpty) queue.right.head else queue.left.head
    override def init[A](queue: HQueue[A]): HQueue[A] = if (queue.left.isEmpty) empty else normD(queue.size - 1, queue.left.tail, queue.right)
    override def tail[A](queue: HQueue[A]): HQueue[A] = if (queue.right.isEmpty) empty else normD(queue.size - 1, queue.left, queue.right.tail)
    override def cons[A](x: A, q: HQueue[A]): HQueue[A] = HQueue(q.size + 1, x :: q.left, q.right)
    override def snoc[A](q: HQueue[A], y: A): HQueue[A] = HQueue(q.size + 1, q.left, y :: q.right)
    override def lengthSeq[A](q: HQueue[A]): Int = q.size
    def fold[A, B](q: HQueue[A])(z: B)(f: (B, A) => B): B =
      q.right.reverse.foldLeft(q.left.foldLeft(z)(f))(f)
    override def toList[A](q: HQueue[A]): List[A] = q.left ++ q.right.reverse
    override def toSeq[A](xs: List[A]): HQueue[A] = normD(xs.length, xs, Nil)
  }

  implicit val hQueueCSequenceInstance: CSequence[HQueue] = new CSequence[HQueue] {
    override def sequence: Sequence[HQueue] = hQueueSequenceInstance
    override def concat[A](fst: HQueue[A], snd: HQueue[A]): HQueue[A] = {
      normD(fst.size + snd.size, fst.left ++ fst.right.reverse ++ snd.left, snd.right)
    }
  }

  def normD[A](size: Int, left: List[A], right: List[A]): HQueue[A] = {
    if (size <= 1) {
      HQueue(size, left, right)
    } else {
      if (left.isEmpty) {
        val (r, f) = right.splitAt(size / 2)
        HQueue(size, f.reverse, r)
      } else if (right.isEmpty) {
        val (f, r) = left.splitAt(size / 2)
        HQueue(size, f, r.reverse)
      } else {
        HQueue(size, left, right)
      }
    }
  }
}



