package scabs.seq

// TurtleQ with cons/snoc leaves in queues unfolded
sealed trait TurtleQI[A] {

  import TurtleQI._

  // fixme: size is being calculated incorrecty for append nodes
  def size: Int

  def conswise: TurtleQI[A]

  def snocwise: TurtleQI[A]

  def isEmpty: Boolean

  // note: when appending, never tosnoc or tocons anything -- we want to attempt to .reverse to the last moment
  def ++(rhs: TurtleQI[A]): TurtleQI[A]

  def :+(a: A): NonEmpty[A]

  def +:(a: A): NonEmpty[A]

  def headTail: Option[(A, TurtleQI[A])]

  def firstLast: Option[(TurtleQI[A], A)]

  def map[B](f: A => B): TurtleQI[B]

  def fold[B](zero: B)(f: (B, A) => B): B

  def renderStructure(sb: StringBuilder): Unit
}


object TurtleQI {

  implicit val sequence: Sequence[TurtleQI] =
    new Sequence[TurtleQI] {
      override def empty[A]: TurtleQI[A] = TNil()

      override def isEmpty[A](q: TurtleQI[A]): Boolean = q.isEmpty

      override def head[A](queue: TurtleQI[A]): A = queue.headTail.get._1

      override def last[A](queue: TurtleQI[A]): A = queue.firstLast.get._2

      override def init[A](queue: TurtleQI[A]): TurtleQI[A] = queue.firstLast.get._1

      override def tail[A](queue: TurtleQI[A]): TurtleQI[A] = queue.headTail.get._2

      override def cons[A](x: A, q: TurtleQI[A]): TurtleQI[A] = x +: q

      override def snoc[A](q: TurtleQI[A], y: A): TurtleQI[A] = q :+ y

      override def lengthSeq[A](q: TurtleQI[A]): Int = q.size

      override def fold[A, B](q: TurtleQI[A])(z: B)(f: (B, A) => B): B = {
//        val sb = new StringBuilder()
//        q.renderStructure(sb)
//        println(s"Folding over structure: $sb")
        q.fold(z)(f)
      }

      override def toList[A](q: TurtleQI[A]): List[A] =
        q.fold[List[A]](Nil)((e, i) => i :: e)

      override def toSeq[A](xs: List[A]): TurtleQI[A] =
        xs.foldLeft[TurtleQI[A]](TNil())((e, i) => i +: e)

      override def uncons[A](s: TurtleQI[A]): Option[(A, TurtleQI[A])] = s.headTail

      override def unsnoc[A](s: TurtleQI[A]): Option[(TurtleQI[A], A)] = s.firstLast

      override def map[A, B](q: TurtleQI[A])(f: (A) => B): TurtleQI[B] = q.map(f)

      override def foreach[A, U](q: TurtleQI[A])(f: (A) => U): Unit = q.map(f)

      override def concat[A](fst: TurtleQI[A], snd: TurtleQI[A]): TurtleQI[A] = {
        val c = fst ++ snd
//
//        val sbf = new StringBuilder()
//        val sbs = new StringBuilder()
//        val sbc = new StringBuilder()
//        fst.renderStructure(sbf)
//        snd.renderStructure(sbs)
//        c.renderStructure(sbc)
//
//        println(s"fst: $sbf")
//        println(s"snd: $sbs")
//        println(s"cat: $sbc")

        c
      }
    }

  val _tnil = TNil[Nothing]()
  def tnil[A] = _tnil.asInstanceOf[TNil[A]]
  def empty[A]: Empty[A] = tnil.asInstanceOf[Empty[A]]

  def sizeAppends[A](appends: NonEmpty[NonEmpty[A]]): Int = appends.fold(0)((i, e) => e.size + i)

  sealed trait Empty[A] extends TurtleQI[A] with ConsOrEmpty[A] with SnocOrEmpty[A] {
    final override def isEmpty: Boolean = true
  }

  sealed trait NonEmpty[A] extends TurtleQI[A] {
    final override def isEmpty: Boolean = false

    final override def headTail: Some[(A, TurtleQI[A])] = Some(headTailNonEmpty)

    final override def firstLast: Some[(TurtleQI[A], A)] = Some(firstLastNonEmpty)

    final override def ++(rhs: TurtleQI[A]): NonEmpty[A] = rhs match {
      case TNil() => this
      case r: NonEmpty[A] => this +:+ r
    }

    def +:+(rhs: NonEmpty[A]): NonEmpty[A]

    def headTailNonEmpty: (A, TurtleQI[A])

    def firstLastNonEmpty: (TurtleQI[A], A)

    override def conswise: NonEmpty[A]

    override def snocwise: NonEmpty[A]

    override def map[B](f: A => B): NonEmpty[B]
  }

  sealed trait ConsOrEmpty[A] extends TurtleQI[A]

  sealed trait SnocOrEmpty[A] extends TurtleQI[A]

  final case class TNil[A]() extends Empty[A] {

    override def size = 0

    override def conswise: TNil[A] = this

    override def snocwise: TNil[A] = this

    override def ++(rhs: TurtleQI[A]): TurtleQI[A] = rhs

    override def :+(a: A): Snoc[A] = Snoc(Nil, a)

    override def +:(a: A): Cons[A] = Cons(a, Nil)

    override def headTail: None.type = None

    override def firstLast: None.type = None

    override def map[B](f: A => B): TNil[B] = tnil.asInstanceOf[TNil[B]]

    override def fold[B](zero: B)(f: (B, A) => B): B = zero

    override def renderStructure(sb: StringBuilder): Unit = sb.append("()")
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
        TqCA(head, tails, r +: empty)
      case Snoc(f, l) =>
        TqCS(head, tails, f, l)
      case TqCA(h, t, a) =>
        TqCA(head, tails, Cons(h, t) +: a)
      case TqCAS(h, t, a, f, l) =>
        TqCAS(head, tails, Cons(h, t) +: a, f, l)
      case TqCS(h, t, f, l) =>
        TqCAS(head, tails, Cons(h, t) +: empty, f, l)
      case TqA(a) =>
        TqCA(head, tails, a)
      case TqAS(a, f, l) =>
        TqCAS(head, tails, a, f, l)
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

    override def renderStructure(sb: StringBuilder): Unit = sb.append("C").append(size)
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
      case Snoc(f, l) =>
        TqAS(empty :+ this, f, l)
      case TqCA(h, t, a) =>
        TqA(this +: Cons(h, t) +: a)
      case TqCAS(h, t, a, f, l) =>
        TqAS(this +: Cons(h, t) +: a, f, l)
      case TqCS(h, t, f, l) =>
        TqAS(this +: Cons(h, t) +: empty[NonEmpty[A]], f, l)
      case TqA(a) =>
        TqA(this +: a)
      case TqAS(a, f, l) =>
        TqAS(this +: a, f, l)
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

    override def renderStructure(sb: StringBuilder): Unit = sb.append("S").append(size)
  }

  final case class TqCA[A](head: A, tails: List[A], appends: NonEmpty[NonEmpty[A]]) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends) + 1 + tails.size

    override def conswise: TqCA[A] = TqCA(head, tails, appends.conswise)

    override def snocwise: TqA[A] = TqA(Cons(head, tails).snocwise +: appends.snocwise)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqCA(head, tails, appends :+ r)
      case Snoc(f, l) =>
        TqCAS(head, tails, appends, f, l)
      case TqCA(hr, tr, ar) =>
        TqCA(head, tails, appends +:+ (Cons(hr, tr) +: ar))
      case TqCAS(h, t, ar, f, l) =>
        TqCAS(head, tails, appends +:+ (Cons(h, t) +: ar), f, l)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(head, tails, appends :+ Cons(hr, tr), fr, lr)
      case TqA(ra) =>
        TqCA(head, tails, appends +:+ ra)
      case TqAS(ra, f, l) =>
        TqCAS(head, tails, appends +:+ ra, f, l)
    }

    override def :+(a: A): TqCAS[A] = TqCAS(head, tails, appends, Nil, a)

    override def +:(a: A): TqCA[A] = TqCA(a, head::tails, appends)

    override def headTailNonEmpty: (A, TurtleQI[A]) = tails match {
      case Nil =>
        (head, TqA(appends))
      case h::t =>
        (head, TqCA(h, t, appends))
    }

    override def firstLastNonEmpty: (TurtleQI[A], A) = appends.firstLastNonEmpty match {
      case (TNil(), al) =>
        al.firstLastNonEmpty match {
          case (TNil(), l) =>
            (Cons(head, tails), l)
          case (f : NonEmpty[A], l) =>
            (Cons(head, tails) +:+ f, l)
        }
      case (af: NonEmpty[NonEmpty[A]], al) =>
        al.firstLastNonEmpty match {
          case (TNil(), l) =>
            (TqCA(head, tails, af), l)
          case (f@Cons(_, _), l) =>
            val s = f.snocwise
            (TqCAS(head, tails, af, s.firsts, s.last), l)
          case (Snoc(fs, ls), l) =>
            (TqCAS(head, tails, af, fs, ls), l)
          case (f: NonEmpty[A], l) =>
            (TqCA(head, tails, af :+ f), l)
        }
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqCA(f(head), tails map f, appends map (_ map f))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val h = f(zero, head)
      val c = tails.foldLeft(h)(f)
      val a = appends.fold(c)((z, e) => e.fold(z)(f))
      a
    }

    override def renderStructure(sb: StringBuilder): Unit = {
      sb.append("<")
      sb.append(1 + tails.size)
      sb.append(",")
      appends.renderStructure(sb)
      sb.append(",_")
      sb.append(">")
    }
  }

  final case class TqCAS[A](head: A, tails: List[A], appends: NonEmpty[NonEmpty[A]], firsts: List[A], last: A) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends) + 1 + tails.size + firsts.size + 1

    override def conswise: TqCA[A] = TqCA(head, tails, appends.conswise :+ Snoc(firsts, last).conswise)

    override def snocwise: TqAS[A] = TqAS(Cons(head, tails).snocwise +: appends.snocwise, firsts, last)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqCA(head, tails, appends :+ Snoc(firsts, last) :+ r)
      case Snoc(f, l) =>
        TqCAS(head, tails, appends :+ Snoc(firsts, last), f, l)
      case TqCA(ch, ct, ar) =>
        TqCA(head, tails, (appends :+ Snoc(firsts, last)) +:+ (Cons(ch, ct) +: ar))
      case TqCAS(h, t, ar, f, l) =>
        TqCAS(head, tails, (appends :+ Snoc(firsts, last)) +:+ (Cons(h, t) +: ar), f, l)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(head, tails, appends :+ Snoc(firsts, last) :+ Cons(hr, tr), fr, lr)
      case TqA(ar) =>
        TqCA(head, tails, (appends :+ Snoc(firsts, last)) +:+ ar)
      case TqAS(ar, f, l) =>
        TqCAS(head, tails, (appends :+ Snoc(firsts, last)) +:+ ar, f, l)
    }

    override def :+(a: A): TqCAS[A] = TqCAS(head, tails, appends, last::firsts, a)

    override def +:(a: A): TqCAS[A] = TqCAS(a, head::tails, appends, firsts, last)

    override def headTailNonEmpty: (A, TurtleQI[A]) = tails match {
      case Nil =>
        (head, TqAS(appends, firsts, last))
      case h::t =>
        (head, TqCAS(h, t, appends, firsts, last))
    }

    override def firstLastNonEmpty: (TurtleQI[A], A) = firsts match {
      case Nil =>
        (TqCA(head, tails, appends), last)
      case l::f =>
        (TqCAS(head, tails, appends, f, l), last)
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqCAS(f(head), tails map f, appends map (_ map f), firsts map f, f(last))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val fh = f(zero, head)
      val ft = tails.foldLeft(fh)(f)
      val fa = appends.fold(ft)((z, e) => e.fold(z)(f))
      val ff = firsts.foldLeft(fa)(f)
      val fl = f(ff, last)
      fl
    }

    override def renderStructure(sb: StringBuilder): Unit = {
      sb.append("<")
      sb.append(1 + tails.size)
      sb.append(",")
      appends.renderStructure(sb)
      sb.append(",")
      sb.append(firsts.size + 1)
      sb.append(">")
    }
  }

  final case class TqCS[A](head: A, tails: List[A], firsts: List[A], last: A) extends NonEmpty[A] {

    override def size: Int = 1 + tails.size + firsts.size + 1

    override def conswise: TqCA[A] = TqCA(head, tails, empty :+ Snoc(firsts, last).conswise)

    override def snocwise: TqAS[A] = TqAS(Cons(head, tails).snocwise +: empty, firsts, last)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqCA(head, tails, Snoc(firsts, last) +: r +: empty[NonEmpty[A]]) // choice to bias to left
      case Snoc(f, l) =>
        TqCAS(head, tails, Snoc(firsts, last) +: empty, f, l)
      case TqCA(hr, tr, ar) =>
        TqCA(head, tails, Snoc(firsts, last) +: Cons(hr, tr) +: ar)
      case TqCAS(hr, tr, ar, f, l) =>
        TqCAS(head, tails, Snoc(firsts, last) +: Cons(hr, tr) +: ar, f, l)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(head, tails, Snoc(firsts, last) +: Cons(hr, tr) +: empty[NonEmpty[A]], fr, lr)
      case TqA(ar) =>
        TqCA(head, tails, Snoc(firsts, last) +: ar)
      case TqAS(ar, f, l) =>
        TqCAS(head, tails, Snoc(firsts, last) +: ar, f, l)
    }

    override def :+(a: A): TqCS[A] = TqCS(head, tails, last :: firsts, a)

    override def +:(a: A): TqCS[A] = TqCS(a, head :: tails, firsts, last)

    override def headTailNonEmpty: (A, TurtleQI[A]) = {
      tails match {
        case Nil =>
          (head, Snoc(firsts, last))
        case th::tt =>
          (head, TqCS(th, tt, firsts, last))
      }
    }

    override def firstLastNonEmpty: (TurtleQI[A], A) = {
      firsts match {
        case Nil =>
          (Cons(head, tails), last)
        case fh::ft =>
          (TqCS(head, tails, ft, fh), last)
      }
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqCS(f(head), tails map f, firsts map f, f(last))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val fh = f(zero, head)
      val fc = tails.foldLeft(fh)(f)
      val ff = firsts.foldLeft(fc)(f)
      val fl = f(ff, last)
      fl
    }

    override def renderStructure(sb: StringBuilder): Unit = {
      sb.append("<")
      sb.append(1 + tails.size)
      sb.append(",_,")
      sb.append(1 + firsts.size)
      sb.append(">")
    }
  }

  final case class TqA[A](appends: NonEmpty[NonEmpty[A]]) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends)

    override def conswise: TqA[A] = TqA(appends.conswise)

    override def snocwise: TqA[A] = TqA(appends.snocwise)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqA(appends :+ r)
      case Snoc(f, l) =>
        TqAS(appends, f, l)
      case TqCA(hr, tr, ar) =>
        TqA((appends :+ Cons(hr, tr)) +:+ ar)
      case TqCAS(hr, tr, ar, fr, lr) =>
        TqAS((appends :+ Cons(hr, tr)) +:+ ar, fr, lr)
      case TqCS(hr, tr, fr, lr) =>
        TqAS(appends :+ Cons(hr, tr), fr, lr)
      case TqA(ar) =>
        TqA(appends +:+ ar)
      case TqAS(ar, fr, lr) =>
        TqAS(appends +:+ ar, fr, lr)
    }

    override def :+(a: A): TqAS[A] = TqAS(appends, Nil, a)

    override def +:(a: A): TqCA[A] = TqCA(a, Nil, appends)

    override def headTailNonEmpty: (A, TurtleQI[A]) = appends.headTailNonEmpty match {
      case (ah, TNil()) => ah.headTailNonEmpty match {
        case (h, TNil()) =>
          (h, empty)
        case (h, t) =>
          (h, t)
      }
      case (ah, at: NonEmpty[NonEmpty[A]]) => ah.headTailNonEmpty match {
        case (h, TNil()) =>
          (h, TqA(at))
        case (h, Cons(ch, ct)) =>
          (h, TqCA(ch, ct, at))
        case (h, t@Snoc(_, _)) =>
          val s = t.conswise
          (h, TqCA(s.head, s.tails, at))
        case (h, t: NonEmpty[A]) =>
          (h, TqA(t +: at))
      }
    }

    override def firstLastNonEmpty: (TurtleQI[A], A) = appends.firstLastNonEmpty match {
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
          val s = f.snocwise
          (TqAS(af, s.firsts, s.last), l)
        case (Snoc(fs, ls), l) =>
          (TqAS(af, fs, ls), l)
        case (f: NonEmpty[A], l) =>
          (TqA(af :+ f), l)
      }
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqA(appends map (_ map f))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      appends.fold(zero)((z, e) => e.fold(z)(f))
    }

    override def renderStructure(sb: StringBuilder): Unit = {
      sb.append("<_,")
      appends.renderStructure(sb)
      sb.append(",_>")
    }
  }

  final case class TqAS[A](appends: NonEmpty[NonEmpty[A]], firsts: List[A], last: A) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends) + firsts.size + 1

    override def conswise: TqA[A] = TqA(appends.conswise :+ Snoc(firsts, last).conswise)

    override def snocwise: TqAS[A] = TqAS(appends.snocwise, firsts, last)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case r@Cons(_, _) =>
        TqA(appends :+ Snoc(firsts, last) :+ r)
      case Snoc(f, l) =>
        TqAS(appends :+ Snoc(firsts, last), f, l)
      case TqCA(hr, tr, ar) =>
        TqA((appends :+ Snoc(firsts, last)) +:+ (Cons(hr, tr) +: ar))
      case TqCAS(hr, tr, ar, fr, lr) =>
        TqAS((appends :+ Snoc(firsts, last)) +:+ (Cons(hr, tr) +: ar), fr, lr)
      case TqCS(hr, tr, fr, lr) =>
        TqAS(appends :+ Snoc(firsts, last) :+ Cons(hr, tr), fr, lr)
      case TqA(ar) =>
        TqA((appends :+ Snoc(firsts, last)) +:+ ar)
      case TqAS(ar, fr, lr) =>
        TqAS((appends :+ Snoc(firsts, last)) +:+ ar, fr, lr)
    }

    override def :+(a: A): TqAS[A] = TqAS(appends, last::firsts, a)

    override def +:(a: A): TqCAS[A] = TqCAS(a, Nil, appends, firsts, last)

    override def headTailNonEmpty: (A, TurtleQI[A]) = appends.headTailNonEmpty match {
      case (ah, TNil()) =>
        ah.headTailNonEmpty match {
          case (h, TNil()) =>
            (h, Snoc(firsts, last))
          case (h, t) =>
            (h, t ++ Snoc(firsts, last))
        }
      case (ah, at: NonEmpty[NonEmpty[A]]) =>
        ah.headTailNonEmpty match {
          case (h, TNil()) =>
            (h, TqAS(at, firsts, last))
          case (h, Cons(ch, ct)) =>
            (h, TqCAS(ch, ct, at, firsts, last))
          case (h, t@Snoc(_, _)) =>
            val s = t.conswise
            (h, TqCAS(s.head, s.tails, at, firsts, last))
          case (h, t: NonEmpty[A]) =>
            (h, TqAS(t +: at, firsts, last))
        }
    }

    override def firstLastNonEmpty: (TurtleQI[A], A) = firsts match {
      case Nil =>
        (TqA(appends), last)
      case l::f =>
        (TqAS(appends, f, l), last)
    }

    override def map[B](f: (A) => B): NonEmpty[B] = TqAS(appends map (_ map f), firsts map f, f(last))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val a = appends.fold(zero)((z, e) => e.fold(z)(f))
      val ff = firsts.foldLeft(a)(f)
      f(ff, last)
    }

    override def renderStructure(sb: StringBuilder): Unit = {
      sb.append("<_,")
      appends.renderStructure(sb)
      sb.append(",")
      sb.append(firsts.size + 1)
      sb.append(">")
    }
  }

}
