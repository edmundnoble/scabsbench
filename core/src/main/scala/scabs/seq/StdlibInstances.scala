package scabs
package seq

import cats.Eq
import scabs.Util._

import scala.collection.immutable.Queue

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

    override def uncons[A](s: Vector[A]): Option[(A, Vector[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None

    override def unsnoc[A](s: Vector[A]): Option[(Vector[A], A)] =
      if (s.nonEmpty) Some((s.init, s.last)) else None

    override def map[A, B](q: Vector[A])(f: (A) => B): Vector[B] = q.map(f)

    override def concat[A](fst: Vector[A], snd: Vector[A]): Vector[A] = fst ++ snd

    override def foreach[A, U](q: Vector[A])(f: (A) => U): Unit = q.foreach(f)
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

    override def uncons[A](s: List[A]): Option[(A, List[A])] = s match {
      case (x :: xs) => Some((x, xs))
      case _ => None
    }

    override def unsnoc[A](s: List[A]): Option[(List[A], A)] = {
      var init: List[A] = Nil
      var tail = s
      while (tail ne Nil) {
        init = tail.head :: init
        val newTail = tail.tail
        if (newTail eq Nil)
          return Some((init, tail.head))
        tail = newTail
      }
      None
    }

    override def map[A, B](q: List[A])(f: (A) => B): List[B] = q.map(f)

    override def concat[A](fst: List[A], snd: List[A]): List[A] = fst ++ snd

    override def foreach[A, U](q: List[A])(f: (A) => U): Unit = q.foreach(f)
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

    override def uncons[A](s: Queue[A]): Option[(A, Queue[A])] = if (s.nonEmpty) Some((s.head, s.tail)) else None

    override def unsnoc[A](s: Queue[A]): Option[(Queue[A], A)] = if (s.isEmpty) None else Some(s.dequeue.swap)

    override def map[A, B](q: Queue[A])(f: (A) => B): Queue[B] = q.map(f)

    override def concat[A](fst: Queue[A], snd: Queue[A]): Queue[A] = fst ++ snd

    override def foreach[A, U](q: Queue[A])(f: (A) => U): Unit = q.foreach(f)
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

    override def uncons[A](s: Stream[A]): Option[(A, Stream[A])] = if (s.nonEmpty) Some((s.head, s.tail)) else None

    // not a lot of care put into this one
    override def unsnoc[A](s: Stream[A]): Option[(Stream[A], A)] = if (s.nonEmpty) Some((s.init, s.last)) else None

    override def map[A, B](q: Stream[A])(f: (A) => B): Stream[B] = q.map(f)

    override def concat[A](fst: Stream[A], snd: Stream[A]): Stream[A] = fst ++ snd

    override def foreach[A, U](q: Stream[A])(f: (A) => U): Unit = q.foreach(f)
  }

  implicit val functionCategoryTailrec: CategoryTailrec[Function1] = new CategoryTailrec[Function1] {
    override def compose[A, B, C](ab: (A) => B, bc: (B) => C): (A) => C = a => bc(ab(a))

    override def id[A]: (A) => A = a => a

    override def tailRecP[S[_] : Sequence, A, B](seq: TASequence[S, Function, A, B]): (A) => B = { a =>
      var now: Any = a
      seq.foreach(new (Function ~~> BiConst[Unit]#l) {
        override def apply[C, D](fa: C => D): Unit = {
          val faUnsafe = fa.asInstanceOf[Any => Any]
          now = faUnsafe(now)
        }
      })
      now.asInstanceOf[B]
    }
  }

  implicit def kleisliCategoryTailrec[F[_]](implicit F: Monad[F]): CategoryTailrec[KleisliC[F]#l] = new CategoryTailrec[KleisliC[F]#l] {
    override def compose[A, B, C](ab: (A) => F[B], bc: (B) => F[C]): (A) => F[C] = a => F.bind(ab(a))(bc)

    override def id[A]: (A) => F[A] = a => F.pure(a)

    override def tailRecP[S[_] : Sequence, A, B](seq: TASequence[S, KleisliC[F]#l, A, B]): (A) => F[B] = { a =>
      var now: F[Any] = F.pure(a)
      seq.foreach(new (KleisliC[F]#l ~~> BiConst[Unit]#l){
        override def apply[C, D](fa: C => F[D]): Unit = {
          val faUnsafe = fa.asInstanceOf[Any => F[Any]]
          now = F.bind(now)(faUnsafe)
        }
      })
      now.asInstanceOf[F[B]]
    }
  }

  implicit val functionEvaluable: Evaluable[Function1, Id] = new Evaluable[Function1, Id] {
    override def eval[A, B](a: A, arr: A => B): B = arr(a)
  }

  implicit def functionEvaluableFunctor[F[_]](implicit F: Functor[F]): Evaluable[Function1, F] = new Evaluable[Function1, F] {
    override def eval[A, B](a: F[A], arr: A => B): F[B] = F.fmap(a)(arr)
  }

  implicit def functionEvaluableMonad[F[_]](implicit F: Monad[F]): Evaluable[Function1, F] = new Evaluable[Function1, F] {
    override def eval[A, B](a: F[A], arr: A => B): F[B] = F.fmap(a)(arr)
  }

  implicit def kleisliEvaluable[F[_]](implicit F: Monad[F]): Evaluable[KleisliC[F]#l, F] = new Evaluable[KleisliC[F]#l, F] {
    override def eval[A, B](a: F[A], arr: (A) => F[B]): F[B] = F.bind(a)(arr)
  }

  implicit def staticEvaluable[F[_]](implicit F: Applicative[F]): Evaluable[StaticC[F]#l, F] = new Evaluable[StaticC[F]#l, F] {
    override def eval[A, B](a: F[A], arr: F[(A) => B]): F[B] = F.ap(a)(arr)
  }

  implicit def steticEvaluable[F[_]](implicit F: Applicative[F]): Evaluable[SteticC[F]#l, F] = new Evaluable[SteticC[F]#l, F] {
    override def eval[A, B](a: F[A], arr: Stetic[F, A, B]): F[B] =
      arr.fold(F.ap(a), F.fmap(a))
  }

  def steticEvaluableTrans[F[_], G[_]](trans: F ~> G)(implicit G: Applicative[G]): Evaluable[SteticC[F]#l, G] =
    new Evaluable[SteticC[F]#l, G] {
      override def eval[A, B](a: G[A], arr: Stetic[F, A, B]): G[B] =
        arr.fold(fa => G.ap(a)(trans(fa)), G.fmap(a))
    }

}
