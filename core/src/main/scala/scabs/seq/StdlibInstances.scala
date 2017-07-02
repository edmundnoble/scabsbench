package scabs
package seq

import cats._
import cats.implicits._
import scabs.Util._

import scala.collection.immutable.Queue

object StdlibInstances {

  type Endo[A] = A => A

  implicit val funSemiK: SemigroupK[Endo] = new SemigroupK[Endo] {
    def combineK[A](fst: Endo[A], snd: Endo[A]): Endo[A] = fst andThen snd
  }

  implicit val vectorSequenceInstance: Sequence[Vector] = new Sequence[Vector] {
    def empty[A]: Vector[A] = Vector.empty[A]

    def isEmpty[A](q: Vector[A]): Boolean = q.isEmpty

    def head[A](queue: Vector[A]): A = queue.head

    def last[A](queue: Vector[A]): A = queue.last

    def init[A](queue: Vector[A]): Vector[A] = queue.init

    def tail[A](queue: Vector[A]): Vector[A] = queue.tail

    def cons[A](x: A, q: Vector[A]): Vector[A] = x +: q

    def snoc[A](q: Vector[A], y: A): Vector[A] = q :+ y

    def lengthSeq[A](q: Vector[A]): Int = q.length

    def toList[A](q: Vector[A]): List[A] = q.toList

    def toSeq[A](xs: List[A]): Vector[A] = xs.toVector

    def cata[A, B](q: Vector[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)

    def uncons[A](s: Vector[A]): Option[(A, Vector[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None

    def unsnoc[A](s: Vector[A]): Option[(Vector[A], A)] =
      if (s.nonEmpty) Some((s.init, s.last)) else None

    def map[A, B](q: Vector[A])(f: (A) => B): Vector[B] = q.map(f)

    def concat[A](fst: Vector[A], snd: Vector[A]): Vector[A] = fst ++ snd

    def foreach[A, U](q: Vector[A])(f: (A) => U): Unit = q.foreach(f)
  }

  implicit val listSequenceInstance: Sequence[List] = new Sequence[List] {
    def empty[A]: List[A] = List.empty[A]

    def isEmpty[A](q: List[A]): Boolean = q.isEmpty

    def head[A](queue: List[A]): A = queue.head

    def last[A](queue: List[A]): A = queue.last

    def init[A](queue: List[A]): List[A] = queue.init

    def tail[A](queue: List[A]): List[A] = queue.tail

    def cons[A](x: A, q: List[A]): List[A] = x +: q

    def snoc[A](q: List[A], y: A): List[A] = q :+ y

    def lengthSeq[A](q: List[A]): Int = q.length

    def toList[A](q: List[A]): List[A] = q

    def toSeq[A](xs: List[A]): List[A] = xs

    def cata[A, B](q: List[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)

    def uncons[A](s: List[A]): Option[(A, List[A])] = s match {
      case (x :: xs) => Some((x, xs))
      case _ => None
    }

    def unsnoc[A](s: List[A]): Option[(List[A], A)] = {
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

    def map[A, B](q: List[A])(f: (A) => B): List[B] = q.map(f)

    def concat[A](fst: List[A], snd: List[A]): List[A] = fst ++ snd

    def foreach[A, U](q: List[A])(f: (A) => U): Unit = q.foreach(f)
  }

  implicit val queueSequenceInstance: Sequence[Queue] = new Sequence[Queue] {
    def empty[A]: Queue[A] = Queue.empty[A]

    def isEmpty[A](q: Queue[A]): Boolean = q.isEmpty

    def head[A](queue: Queue[A]): A = queue.head

    def last[A](queue: Queue[A]): A = queue.last

    def init[A](queue: Queue[A]): Queue[A] = queue.init

    def tail[A](queue: Queue[A]): Queue[A] = queue.tail

    def cons[A](x: A, q: Queue[A]): Queue[A] = x +: q

    def snoc[A](q: Queue[A], y: A): Queue[A] = q :+ y

    def lengthSeq[A](q: Queue[A]): Int = q.length

    def toList[A](q: Queue[A]): List[A] = q.toList

    def toSeq[A](xs: List[A]): Queue[A] = xs.to[Queue]

    def cata[A, B](q: Queue[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)

    def uncons[A](s: Queue[A]): Option[(A, Queue[A])] = if (s.nonEmpty) Some((s.head, s.tail)) else None

    def unsnoc[A](s: Queue[A]): Option[(Queue[A], A)] = if (s.isEmpty) None else Some(s.dequeue.swap)

    def map[A, B](q: Queue[A])(f: (A) => B): Queue[B] = q.map(f)

    def concat[A](fst: Queue[A], snd: Queue[A]): Queue[A] = fst ++ snd

    def foreach[A, U](q: Queue[A])(f: (A) => U): Unit = q.foreach(f)
  }

  implicit val streamSequenceInstance: Sequence[Stream] = new Sequence[Stream] {
    def empty[A]: Stream[A] = Stream.empty[A]

    def isEmpty[A](q: Stream[A]): Boolean = q.isEmpty

    def head[A](stream: Stream[A]): A = stream.head

    def last[A](stream: Stream[A]): A = stream.last

    def init[A](stream: Stream[A]): Stream[A] = stream.init

    def tail[A](stream: Stream[A]): Stream[A] = stream.tail

    def cons[A](x: A, q: Stream[A]): Stream[A] = x +: q

    def snoc[A](q: Stream[A], y: A): Stream[A] = q :+ y

    def lengthSeq[A](q: Stream[A]): Int = q.length

    def toList[A](q: Stream[A]): List[A] = q.toList

    def toSeq[A](xs: List[A]): Stream[A] = xs.to[Stream]

    def cata[A, B](q: Stream[A])(z: B)(f: (B, A) => B): B = q.foldLeft(z)(f)

    def uncons[A](s: Stream[A]): Option[(A, Stream[A])] = if (s.nonEmpty) Some((s.head, s.tail)) else None

    // not a lot of care put into this one
    def unsnoc[A](s: Stream[A]): Option[(Stream[A], A)] = if (s.nonEmpty) Some((s.init, s.last)) else None

    def map[A, B](q: Stream[A])(f: (A) => B): Stream[B] = q.map(f)

    def concat[A](fst: Stream[A], snd: Stream[A]): Stream[A] = fst ++ snd

    def foreach[A, U](q: Stream[A])(f: (A) => U): Unit = q.foreach(f)
  }

  implicit val functionCategoryTailrec: CategoryTailrec[Function1] = new CategoryTailrec[Function1] {
    def compose[A, B, C](ab: (A) => B, bc: (B) => C): (A) => C = a => bc(ab(a))

    def id[A]: (A) => A = a => a

    def tailRecP[S[_] : Sequence, A, B](seq: TASequence[S, Function, A, B]): (A) => B = { a =>
      var now: Any = a
      seq.foreach(new (Function ~~> BiConst[Unit]#l) {
        def apply[C, D](fa: C => D): Unit = {
          val faUnsafe = fa.asInstanceOf[Any => Any]
          now = faUnsafe(now)
        }
      })
      now.asInstanceOf[B]
    }
  }

  implicit def kleisliCategoryTailrec[F[_]](implicit F: Monad[F]): CategoryTailrec[KleisliC[F]#l] = new CategoryTailrec[KleisliC[F]#l] {
    def compose[A, B, C](ab: (A) => F[B], bc: (B) => F[C]): (A) => F[C] = a => F.flatMap(ab(a))(bc)

    def id[A]: (A) => F[A] = a => F.pure(a)

    def tailRecP[S[_] : Sequence, A, B](seq: TASequence[S, KleisliC[F]#l, A, B]): (A) => F[B] = { a =>
      var now: F[Any] = F.pure(a)
      seq.foreach(new (KleisliC[F]#l ~~> BiConst[Unit]#l){
        def apply[C, D](fa: C => F[D]): Unit = {
          val faUnsafe = fa.asInstanceOf[Any => F[Any]]
          now = F.flatMap(now)(faUnsafe)
        }
      })
      now.asInstanceOf[F[B]]
    }
  }

  implicit val functionEvaluable: Evaluable[Function1, Id] = new Evaluable[Function1, Id] {
    def eval[A, B](a: A, arr: A => B): B = arr(a)
  }

  implicit def functionEvaluableFunctor[F[_]](implicit F: Functor[F]): Evaluable[Function1, F] = new Evaluable[Function1, F] {
    def eval[A, B](a: F[A], arr: A => B): F[B] = F.map(a)(arr)
  }

//  implicit def functionEvaluableMonad[F[_]](implicit F: Monad[F]): Evaluable[Function1, F] = new Evaluable[Function1, F] {
//    def eval[A, B](a: F[A], arr: A => B): F[B] = F.map(a)(arr)
//  }

  implicit def kleisliEvaluable[F[_]](implicit F: Monad[F]): Evaluable[KleisliC[F]#l, F] = new Evaluable[KleisliC[F]#l, F] {
    def eval[A, B](a: F[A], arr: (A) => F[B]): F[B] = F.flatMap(a)(arr)
  }

  implicit def staticEvaluable[F[_]](implicit F: Applicative[F]): Evaluable[StaticC[F]#l, F] = new Evaluable[StaticC[F]#l, F] {
    def eval[A, B](a: F[A], arr: F[(A) => B]): F[B] = F.ap(arr)(a)
  }

  implicit def steticEvaluable[F[_]](implicit F: Applicative[F]): Evaluable[SteticC[F]#l, F] = new Evaluable[SteticC[F]#l, F] {
    def eval[A, B](a: F[A], arr: Stetic[F, A, B]): F[B] =
      arr.fold(F.ap(_)(a), F.map(a))
  }

  def steticEvaluableTrans[F[_], G[_]](trans: F ~> G)(implicit G: Applicative[G]): Evaluable[SteticC[F]#l, G] =
    new Evaluable[SteticC[F]#l, G] {
      def eval[A, B](a: G[A], arr: Stetic[F, A, B]): G[B] =
        arr.fold(fa => G.ap(trans(fa))(a), G.map(a))
    }

}
