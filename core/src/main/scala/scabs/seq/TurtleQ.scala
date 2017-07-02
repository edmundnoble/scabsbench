package scabs
package seq

sealed trait TurtleQ[A] {

  import TurtleQ._

  // fixme: size is being calculated incorrecty for append nodes
  def size: Int

  def conswise: TurtleQ[A]

  def snocwise: TurtleQ[A]

  def isEmpty: Boolean

  // note: when appending, never tosnoc or tocons anything -- we want to attempt to .reverse to the last moment
  def ++(rhs: TurtleQ[A]): TurtleQ[A]

  def :+(a: A): NonEmpty[A]

  def +:(a: A): NonEmpty[A]

  def headTail: Option[(A, TurtleQ[A])]

  def firstLast: Option[(TurtleQ[A], A)]

  def map[B](f: A => B): TurtleQ[B]

  def fold[B](zero: B)(f: (B, A) => B): B
}


object TurtleQ {
  val tnil = TNil[Nothing]()

  implicit val sequence: Sequence[TurtleQ] =
    new Sequence[TurtleQ] {
      override def empty[A]: TurtleQ[A] = TNil()

      override def isEmpty[A](q: TurtleQ[A]): Boolean = q.isEmpty

      override def head[A](queue: TurtleQ[A]): A = queue.headTail.get._1

      override def last[A](queue: TurtleQ[A]): A = queue.firstLast.get._2

      override def init[A](queue: TurtleQ[A]): TurtleQ[A] = queue.firstLast.get._1

      override def tail[A](queue: TurtleQ[A]): TurtleQ[A] = queue.headTail.get._2

      override def cons[A](x: A, q: TurtleQ[A]): TurtleQ[A] = x +: q

      override def snoc[A](q: TurtleQ[A], y: A): TurtleQ[A] = q :+ y

      override def lengthSeq[A](q: TurtleQ[A]): Int = q.size

      override def cata[A, B](q: TurtleQ[A])(z: B)(f: (B, A) => B): B = q.fold(z)(f)

      override def toList[A](q: TurtleQ[A]): List[A] =
        q.fold[List[A]](Nil)((e, i) => i :: e)

      override def toSeq[A](xs: List[A]): TurtleQ[A] =
        xs.foldLeft[TurtleQ[A]](TNil())((e, i) => i +: e)

      override def uncons[A](s: TurtleQ[A]): Option[(A, TurtleQ[A])] = s.headTail

      override def unsnoc[A](s: TurtleQ[A]): Option[(TurtleQ[A], A)] = s.firstLast

      override def map[A, B](q: TurtleQ[A])(f: (A) => B): TurtleQ[B] = q.map(f)

      override def foreach[A, U](q: TurtleQ[A])(f: (A) => U): Unit = q.map(f)

      override def concat[A](fst: TurtleQ[A], snd: TurtleQ[A]): TurtleQ[A] = fst ++ snd
    }

  def empty[A]: Empty[A] = tnil.asInstanceOf[Empty[A]]

  def sizeAppends[A](appends: NonEmpty[NonEmpty[A]]): Int = appends.fold(0)((i, e) => e.size + i)

  sealed trait Empty[A] extends TurtleQ[A] with ConsOrEmpty[A] with SnocOrEmpty[A] {
    final override def isEmpty: Boolean = true
  }

  sealed trait NonEmpty[A] extends TurtleQ[A] {
    final override def isEmpty: Boolean = false

    final override def headTail: Some[(A, TurtleQ[A])] = Some(headTailNonEmpty)

    final override def firstLast: Some[(TurtleQ[A], A)] = Some(firstLastNonEmpty)

    final override def ++(rhs: TurtleQ[A]): NonEmpty[A] = rhs match {
      case TNil() => this
      case r: NonEmpty[A] => this +:+ r
    }

    def +:+(rhs: NonEmpty[A]): NonEmpty[A]

    def headTailNonEmpty: (A, TurtleQ[A])

    def firstLastNonEmpty: (TurtleQ[A], A)

    override def conswise: NonEmpty[A]

    override def snocwise: NonEmpty[A]

    override def map[B](f: A => B): NonEmpty[B]
  }

  sealed trait ConsOrSnoc[A] extends TurtleQ[A]

  sealed trait ConsOrEmpty[A] extends TurtleQ[A]

  sealed trait SnocOrEmpty[A] extends TurtleQ[A]

  final case class TNil[A]() extends Empty[A] {

    override def size = 0

    override def conswise: TNil[A] = this

    override def snocwise: TNil[A] = this

    override def ++(rhs: TurtleQ[A]): TurtleQ[A] = rhs

    override def :+(a: A): Snoc[A] = Snoc(Nil, a)

    override def +:(a: A): Cons[A] = Cons(a, Nil)

    override def headTail: None.type = None

    override def firstLast: None.type = None

    override def map[B](f: A => B): TNil[B] = tnil.asInstanceOf[TNil[B]]

    override def fold[B](zero: B)(f: (B, A) => B): B = zero
  }

  final case class Cons[A](head: A, tails: List[A]) extends NonEmpty[A] with ConsOrEmpty[A] {

    override def size = 1 + tails.size

    override def conswise: Cons[A] = this

    override def snocwise: Snoc[A] = {
      val l :: f = (head :: tails).reverse
      Snoc(f, l)
    }

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqCA(this, r +: empty)
      case Snoc(f, l) =>
        TqCS(head, tails, f, l)
      case TqCA(c, a) =>
        TqCA(this, c +: a)
      case TqCAS(c, a, s) =>
        TqCAS(this, c +: a, s)
      case TqCS(h, t, f, l) =>
        TqCAS(this, Cons(h, t) +: empty, Snoc(f, l))
      case TqA(a) =>
        TqCA(this, a)
      case TqAS(a, s) =>
        TqCAS(this, a, s)
    }

    override def :+(a: A): TqCS[A] = TqCS(head, tails, Nil, a)

    override def +:(a: A): Cons[A] = Cons(a, head :: tails)

    override def headTailNonEmpty: (A, ConsOrEmpty[A]) = {
      (head,
        tails match {
          case Nil =>
            empty[A]
          case h :: t =>
            Cons(h, t)
        }
      )
    }

    override def firstLastNonEmpty: (SnocOrEmpty[A], A) = snocwise.firstLastNonEmpty

    override def map[B](f: (A) => B): Cons[B] = Cons(f(head), tails map f)

    override def fold[B](zero: B)(f: (B, A) => B): B = tails.foldLeft(f(zero, head))(f)
  }

  final case class Snoc[A](firsts: List[A], last: A) extends NonEmpty[A] with SnocOrEmpty[A] {

    override def size = firsts.size + 1

    override def conswise: Cons[A] = {
      val h :: t = (last :: firsts).reverse
      Cons(h, t)
    }

    override def snocwise: Snoc[A] = this

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqA(this +: r +: empty[NonEmpty[A]]) // choice to bias to left
      case r@Snoc(_, _) =>
        TqAS(empty :+ this, r)
      case TqCA(c, a) =>
        TqA(this +: c +: a)
      case TqCAS(c, a, s) =>
        TqAS(this +: c +: a, s)
      case TqCS(h, t, f, l) =>
        TqAS(this +: Cons(h, t) +: empty[NonEmpty[A]], Snoc(f, l))
      case TqA(a) =>
        TqA(this +: a)
      case TqAS(a, s) =>
        TqAS(this +: a, s)
    }

    override def :+(a: A): Snoc[A] = Snoc(last :: firsts, a)

    override def +:(a: A): TqCS[A] = TqCS(a, Nil, firsts, last)

    override def headTailNonEmpty: (A, ConsOrEmpty[A]) = conswise.headTailNonEmpty

    override def firstLastNonEmpty: (SnocOrEmpty[A], A) = {
      (firsts match {
        case Nil =>
          empty[A]
        case l :: f => Snoc(f, l)
      },
        last
      )
    }

    override def map[B](f: (A) => B): Snoc[B] = Snoc(firsts map f, f(last))

    override def fold[B](zero: B)(f: (B, A) => B): B = firsts.foldLeft(f(zero, last))(f)
  }

  final case class TqCA[A](cons: Cons[A], appends: NonEmpty[NonEmpty[A]]) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends) + cons.size

    override def conswise: TqCA[A] = TqCA(cons, appends.conswise)

    override def snocwise: TqA[A] = TqA(cons.snocwise +: appends.snocwise)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqCA(cons, appends :+ r)
      case r@Snoc(_, _) =>
        TqCAS(cons, appends, r)
      case TqCA(cr, ar) =>
        TqCA(cons, appends +:+ (cr +: ar))
      case TqCAS(cr, ar, sr) =>
        TqCAS(cons, appends +:+ (cr +: ar), sr)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(cons, appends :+ Cons(hr, tr), Snoc(fr, lr))
      case TqA(ra) =>
        TqCA(cons, appends +:+ ra)
      case TqAS(ra, rs) =>
        TqCAS(cons, appends +:+ ra, rs)
    }

    override def :+(a: A): TqCAS[A] = TqCAS(cons, appends, Snoc(Nil, a))

    override def +:(a: A): TqCA[A] = TqCA(a +: cons, appends)

    override def headTailNonEmpty: (A, TurtleQ[A]) = cons.headTailNonEmpty match {
      case (h, TNil()) =>
        (h, TqA(appends))
      case (h, t@Cons(_, _)) =>
        (h, TqCA(t, appends))
    }

    override def firstLastNonEmpty: (TurtleQ[A], A) = appends.firstLastNonEmpty match {
      case (TNil(), al) =>
        al.firstLastNonEmpty match {
          case (TNil(), l) =>
            (cons, l)
          case (f, l) =>
            (cons ++ f, l)
        }
      case (af: NonEmpty[NonEmpty[A]], al) =>
        al.firstLastNonEmpty match {
          case (TNil(), l) =>
            (TqCA(cons, af), l)
          case (f@Cons(_, _), l) =>
            (TqCAS(cons, af, f.snocwise), l)
          case (f@Snoc(_, _), l) =>
            (TqCAS(cons, af, f), l)
          case (f: NonEmpty[A], l) =>
            (TqCA(cons, af :+ f), l)
        }
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqCA(cons map f, appends map (_ map f))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val c = cons.fold(zero)(f)
      val a = appends.fold(c)((z, e) => e.fold(z)(f))
      a
    }
  }

  final case class TqCAS[A](cons: Cons[A], appends: NonEmpty[NonEmpty[A]], snoc: Snoc[A]) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends) + cons.size + snoc.size

    override def conswise: TqCA[A] = TqCA(cons, appends.conswise :+ snoc.conswise)

    override def snocwise: TqAS[A] = TqAS(cons.snocwise +: appends.snocwise, snoc)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqCA(cons, appends :+ snoc :+ r)
      case r@Snoc(_, _) =>
        TqCAS(cons, appends :+ snoc, r)
      case TqCA(cr, ar) =>
        TqCA(cons, (appends :+ snoc) +:+ (cr +: ar))
      case TqCAS(cr, ar, sr) =>
        TqCAS(cons, (appends :+ snoc) +:+ (cr +: ar), sr)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(cons, appends :+ snoc :+ Cons(hr, tr), Snoc(fr, lr))
      case TqA(ar) =>
        TqCA(cons, (appends :+ snoc) +:+ ar)
      case TqAS(ar, sr) =>
        TqCAS(cons, (appends :+ snoc) +:+ ar, sr)
    }

    override def :+(a: A): TqCAS[A] = TqCAS(cons, appends, snoc :+ a)

    override def +:(a: A): TqCAS[A] = TqCAS(a +: cons, appends, snoc)

    override def headTailNonEmpty: (A, TurtleQ[A]) = cons.headTailNonEmpty match {
      case (h, TNil()) =>
        (h, TqAS(appends, snoc))
      case (h, t@Cons(_, _)) =>
        (h, TqCAS(t, appends, snoc))
    }

    override def firstLastNonEmpty: (TurtleQ[A], A) = snoc.firstLastNonEmpty match {
      case (TNil(), l) =>
        (TqCA(cons, appends), l)
      case (f@Snoc(_, _), l) =>
        (TqCAS(cons, appends, f), l)
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqCAS(cons map f, appends map (_ map f), snoc map f)

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val c = cons.fold(zero)(f)
      val a = appends.fold(c)((z, e) => e.fold(z)(f))
      val s = snoc.fold(a)(f)
      s
    }
  }

  final case class TqCS[A](head: A, tails: List[A], firsts: List[A], last: A) extends NonEmpty[A] {

    override def size: Int = 1 + tails.size + firsts.size + 1

    def cons = Cons(head, tails)
    def snoc = Snoc(firsts, last)

    override def conswise: TqCA[A] = TqCA(cons, empty :+ snoc.conswise)

    override def snocwise: TqAS[A] = TqAS(cons.snocwise +: empty, snoc)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqCA(cons, snoc +: r +: empty[NonEmpty[A]]) // choice to bias to left
      case r@Snoc(_, _) =>
        TqCAS(cons, snoc +: empty, r)
      case TqCA(cr, ar) =>
        TqCA(cons, snoc +: cr +: ar)
      case TqCAS(cr, ar, sr) =>
        TqCAS(cons, snoc +: cr +: ar, sr)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(cons, snoc +: Cons(hr, tr) +: empty[NonEmpty[A]], Snoc(fr, lr))
      case TqA(ar) =>
        TqCA(cons, snoc +: ar)
      case TqAS(ar, as) =>
        TqCAS(cons, snoc +: ar, as)
    }

    override def :+(a: A): TqCS[A] = TqCS(head, tails, last :: firsts, a)

    override def +:(a: A): TqCS[A] = TqCS(a, head :: tails, firsts, last)

    override def headTailNonEmpty: (A, TurtleQ[A]) = {
      tails match {
        case Nil =>
          (head, snoc)
        case th::tt =>
          (head, TqCS(th, tt, firsts, last))
      }
    }

    override def firstLastNonEmpty: (TurtleQ[A], A) = {
      firsts match {
        case Nil =>
          (cons, last)
        case fh::ft =>
          (TqCS(head, tails, ft, fh), last)
      }
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqCS(f(head), tails map f, firsts map f, f(last))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      // this is incorrect
      val c = cons.fold(zero)(f)
      val s = snoc.fold(c)(f)
      s
    }
  }

  final case class TqA[A](appends: NonEmpty[NonEmpty[A]]) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends)

    override def conswise: TqA[A] = TqA(appends.conswise)

    override def snocwise: TqA[A] = TqA(appends.snocwise)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqA(appends :+ r)
      case r@Snoc(_, _) =>
        TqAS(appends, r)
      case TqCA(cr, ar) =>
        TqA((appends :+ cr) +:+ ar)
      case TqCAS(cr, ar, sr) =>
        TqAS((appends :+ cr) +:+ ar, sr)
      case TqCS(hr, tr, fr, lr) =>
        TqAS(appends :+ Cons(hr, tr), Snoc(fr, lr))
      case TqA(ar) =>
        TqA(appends +:+ ar)
      case TqAS(ar, sr) =>
        TqAS(appends +:+ ar, sr)
    }

    override def :+(a: A): TqAS[A] = TqAS(appends, Snoc(Nil, a))

    override def +:(a: A): TqCA[A] = TqCA(Cons(a, Nil), appends)

    override def headTailNonEmpty: (A, TurtleQ[A]) = appends.headTailNonEmpty match {
      case (ah, TNil()) => ah.headTailNonEmpty match {
        case (h, TNil()) =>
          (h, empty)
        case (h, t) =>
          (h, t)
      }
      case (ah, at: NonEmpty[NonEmpty[A]]) => ah.headTailNonEmpty match {
        case (h, TNil()) =>
          (h, TqA(at))
        case (h, t@Cons(_, _)) =>
          (h, TqCA(t, at))
        case (h, t@Snoc(_, _)) =>
          (h, TqCA(t.conswise, at))
        case (h, t: NonEmpty[A]) =>
          (h, TqA(t +: at))
      }
    }

    override def firstLastNonEmpty: (TurtleQ[A], A) = appends.firstLastNonEmpty match {
      case (TNil(), al) => al.firstLastNonEmpty match {
        case (TNil(), l) =>
          (empty, l)
        case (f, l) =>
          (f, l)
      }
      case (af: NonEmpty[NonEmpty[A]], al) => al.firstLastNonEmpty match {
        case (TNil(), l) =>
          (TqA(af), l)
        case (f@Cons(_, _), l) =>
          (TqAS(af, f.snocwise), l)
        case (f@Snoc(_, _), l) =>
          (TqAS(af, f), l)
        case (f: NonEmpty[A], l) =>
          (TqA(af :+ f), l)
      }
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqA(appends map (_ map f))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      appends.fold(zero)((z, e) => e.fold(z)(f))
    }

  }

  final case class TqAS[A](appends: NonEmpty[NonEmpty[A]], snoc: Snoc[A]) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends) + snoc.size

    override def conswise: TqA[A] = TqA(appends.conswise :+ snoc.conswise)

    override def snocwise: TqAS[A] = TqAS(appends.snocwise, snoc)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqA(appends :+ snoc :+ r)
      case r@Snoc(_, _) =>
        TqAS(appends :+ snoc, r)
      case TqCA(cr, ar) =>
        TqA((appends :+ snoc) +:+ (cr +: ar))
      case TqCAS(cr, ar, sr) =>
        TqAS((appends :+ snoc) +:+ (cr +: ar), sr)
      case TqCS(hr, tr, fr, lr) =>
        TqAS(appends :+ snoc :+ Cons(hr, tr), Snoc(fr, lr))
      case TqA(ar) =>
        TqA((appends :+ snoc) +:+ ar)
      case TqAS(ar, sr) =>
        TqAS((appends :+ snoc) +:+ ar, sr)
    }

    override def :+(a: A): TqAS[A] = TqAS(appends, snoc :+ a)

    override def +:(a: A): TqCAS[A] = TqCAS(Cons(a, Nil), appends, snoc)

    override def headTailNonEmpty: (A, TurtleQ[A]) = appends.headTailNonEmpty match {
      case (ah, TNil()) =>
        ah.headTailNonEmpty match {
          case (h, TNil()) =>
            (h, snoc)
          case (h, t) =>
            (h, t ++ snoc)
        }
      case (ah, at: NonEmpty[NonEmpty[A]]) =>
        ah.headTailNonEmpty match {
          case (h, TNil()) =>
            (h, TqAS(at, snoc))
          case (h, t@Cons(_, _)) =>
            (h, TqCAS(t, at, snoc))
          case (h, t@Snoc(_, _)) =>
            (h, TqCAS(t.conswise, at, snoc))
          case (h, t: NonEmpty[A]) =>
            (h, TqAS(t +: at, snoc))
        }
    }

    override def firstLastNonEmpty: (TurtleQ[A], A) = snoc.firstLastNonEmpty match {
      case (TNil(), l) =>
        (TqA(appends), l)
      case (f@Snoc(_, _), l) =>
        (TqAS(appends, snoc), l)
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqAS(appends map (_ map f), snoc map f)

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val a = appends.fold(zero)((z, e) => e.fold(z)(f))
      snoc.fold(a)(f)
    }
  }

}

