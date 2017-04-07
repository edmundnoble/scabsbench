package scabs.seq

// TurtleQS2 with extra snoc-cons fields to enable explicit unity tracking
sealed trait TurtleQS3[A] {

  import TurtleQS3._

  def size: Int

  def conswise: TurtleQS3[A]

  def snocwise: TurtleQS3[A]

  def isEmpty: Boolean

  def ++(rhs: TurtleQS3[A]): TurtleQS3[A]

  def :+(a: A): NonEmpty[A]

  def +:(a: A): NonEmpty[A]

  def headTail: Option[(A, TurtleQS3[A])]

  def firstLast: Option[(TurtleQS3[A], A)]

  def map[B](f: A => B): TurtleQS3[B]

  def fold[B](zero: B)(f: (B, A) => B): B

  def renderStructure(sb: StringBuilder): Unit
}


object TurtleQS3 {
//  var c = 0
//  def shouldLog = {
//    c = c + 1
//    c % 10 == 0
//  }

  implicit val sequence: Sequence[TurtleQS3] =
    new Sequence[TurtleQS3] {
      override def empty[A]: TurtleQS3[A] = TNil()

      override def isEmpty[A](q: TurtleQS3[A]): Boolean = q.isEmpty

      override def head[A](queue: TurtleQS3[A]): A = queue.headTail.get._1

      override def last[A](queue: TurtleQS3[A]): A = queue.firstLast.get._2

      override def init[A](queue: TurtleQS3[A]): TurtleQS3[A] = queue.firstLast.get._1

      override def tail[A](queue: TurtleQS3[A]): TurtleQS3[A] = queue.headTail.get._2

      override def cons[A](x: A, q: TurtleQS3[A]): TurtleQS3[A] = x +: q

      override def snoc[A](q: TurtleQS3[A], y: A): TurtleQS3[A] = q :+ y

      override def lengthSeq[A](q: TurtleQS3[A]): Int = q.size

      override def fold[A, B](q: TurtleQS3[A])(z: B)(f: (B, A) => B): B = {
//        if(shouldLog) {
//          val sb = new StringBuilder()
//          q.renderStructure(sb)
//          println(s"Folding over structure: $sb")
//        }
        q.fold(z)(f)
      }

      override def toList[A](q: TurtleQS3[A]): List[A] =
        q.fold[List[A]](Nil)((e, i) => i :: e)

      override def toSeq[A](xs: List[A]): TurtleQS3[A] =
        xs.foldLeft[TurtleQS3[A]](TNil())((e, i) => i +: e)

      override def uncons[A](s: TurtleQS3[A]): Option[(A, TurtleQS3[A])] = s.headTail

      override def unsnoc[A](s: TurtleQS3[A]): Option[(TurtleQS3[A], A)] = s.firstLast

      override def map[A, B](q: TurtleQS3[A])(f: (A) => B): TurtleQS3[B] = q.map(f)

      override def foreach[A, U](q: TurtleQS3[A])(f: (A) => U): Unit = q.map(f)

      override def concat[A](fst: TurtleQS3[A], snd: TurtleQS3[A]): TurtleQS3[A] = {
        val c = fst ++ snd

//        if(shouldLog) {
//          val sbf = new StringBuilder()
//          val sbs = new StringBuilder()
//          val sbc = new StringBuilder()
//          fst.renderStructure(sbf)
//          snd.renderStructure(sbs)
//          c.renderStructure(sbc)
//
//          println(s"fst: $sbf")
//          println(s"snd: $sbs")
//          println(s"cat: $sbc")
//        }

        c
      }
    }

  val _tnil = TNil[Nothing]()
  def tnil[A] = _tnil.asInstanceOf[TNil[A]]
  def empty[A]: Empty[A] = tnil.asInstanceOf[Empty[A]]

  def sizeAppends[A](appends: NonEmpty[NonEmpty[A]]): Int = appends.fold(0)((i, e) => e.size + i)

  @inline def consOrUnity[A](h0: A, t: List[A]): ConsOrUnity[A] = t match {
    case Nil => Unity(h0)
    case h1::t1 => Cons(h0, h1, t1)
  }

  @inline def snocOrUnity[A](f: List[A], l0: A): SnocOrUnity[A] = f match {
    case Nil => Unity(l0)
    case l1::f1 => Snoc(f1, l1, l0)
  }

  sealed trait Empty[A] extends TurtleQS3[A] with UnityOrEmpty[A] {
    final override def isEmpty: Boolean = true
  }

  sealed trait NonEmpty[A] extends TurtleQS3[A] {
    final override def isEmpty: Boolean = false

    final override def headTail: Some[(A, TurtleQS3[A])] = Some(headTailNonEmpty)

    final override def firstLast: Some[(TurtleQS3[A], A)] = Some(firstLastNonEmpty)

    final override def ++(rhs: TurtleQS3[A]): NonEmpty[A] = rhs match {
      case TNil() => this
      case r: NonEmpty[A] => this +:+ r
    }

    def +:+(rhs: NonEmpty[A]): NonEmpty[A]

    def headTailNonEmpty: (A, TurtleQS3[A])

    def firstLastNonEmpty: (TurtleQS3[A], A)

    override def conswise: NonEmpty[A]

    override def snocwise: NonEmpty[A]

    override def map[B](f: A => B): NonEmpty[B]
  }

  sealed trait ConsOrUnityOrEmpty[A] extends TurtleQS3[A]
  sealed trait SnocOrUnityOrEmpty[A] extends TurtleQS3[A]

  sealed trait UnityOrEmpty[A] extends ConsOrUnityOrEmpty[A] with SnocOrUnityOrEmpty[A]

  sealed trait ConsOrUnity[A] extends ConsOrUnityOrEmpty[A] with NonEmpty[A]

  sealed trait SnocOrUnity[A] extends SnocOrUnityOrEmpty[A] with NonEmpty[A]

  final case class TNil[A]() extends Empty[A] {

    override def size = 0

    override def conswise: TNil[A] = this

    override def snocwise: TNil[A] = this

    override def ++(rhs: TurtleQS3[A]): TurtleQS3[A] = rhs

    override def :+(a: A): Unity[A] = Unity(a)

    override def +:(a: A): Unity[A] = Unity(a)

    override def headTail: None.type = None

    override def firstLast: None.type = None

    override def map[B](f: A => B): TNil[B] = tnil.asInstanceOf[TNil[B]]

    override def fold[B](zero: B)(f: (B, A) => B): B = zero

    override def renderStructure(sb: StringBuilder): Unit = sb.append("()")
  }

  final case class Unity[A](u: A) extends ConsOrUnity[A] with SnocOrUnity[A] with UnityOrEmpty[A] {
    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case Unity(ur) =>
        TqCS(u, Nil, Nil, ur)
      case Cons(h0, h1, t) =>
        Cons(u, h0, h1::t)
      case Snoc(f, l1, l0) =>
        TqCS(u, Nil, l1::f, l0)
      case TqCA(h, t, a) =>
        TqCA(u, h::t, a)
      case TqCAS(h, t, a, f, l) =>
        TqCAS(u, h::t, a, f, l)
      case TqCS(h, t, f, l) =>
        TqCS(u, h::t, f, l)
      case TqA(a) =>
        TqCA(u, Nil, a)
      case TqAS(a, f, l) =>
        TqCAS(u, Nil, a, f, l)
    }

    override def headTailNonEmpty: (A, TNil[A]) = (u, tnil[A])

    override def firstLastNonEmpty: (TNil[A], A) = (tnil[A], u)

    override def conswise: Unity[A] = this

    override def snocwise: Unity[A] = this

    override def map[B](f: (A) => B): Unity[B] = Unity(f(u))

    override def size: Int = 1

    override def :+(a: A): NonEmpty[A] = Snoc(Nil, u, a)

    override def +:(a: A): NonEmpty[A] = Cons(a, u, Nil)

    override def fold[B](zero: B)(f: (B, A) => B): B = f(zero, u)

    override def renderStructure(sb: StringBuilder): Unit = sb.append("(1)")
  }

  final case class Cons[A](head0: A, head1: A, tails: List[A]) extends ConsOrUnity[A] {

    override def size = 2 + tails.size

    override def conswise: Cons[A] = this

    override def snocwise: Snoc[A] = {
      val l0 :: l1 :: f = (head0 :: head1 :: tails).reverse
      Snoc(f, l1, l0)
    }

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case Unity(u) =>
        TqCS(head0, head1::tails, Nil, u)
      case r@Cons(_, _, _) =>
        TqCA(head0, head1::tails, r +: empty)
      case Snoc(f, l1, l0) =>
        TqCS(head0, head1::tails, l1::f, l0)
      case TqCA(h0, t, a) =>
        TqCA(head0, head1::tails, consOrUnity(h0, t) +: a)
      case TqCAS(h, t, a, f, l) =>
        TqCAS(head0, head1::tails, consOrUnity(h, t) +: a, f, l)
      case TqCS(h, t, f, l) =>
        TqCAS(head0, head1::tails, consOrUnity(h, t) +: empty, f, l)
      case TqA(a) =>
        TqCA(head0, head1::tails, a)
      case TqAS(a, f, l) =>
        TqCAS(head0, head1::tails, a, f, l)
    }

    override def :+(a: A): TqCS[A] = TqCS(head0, head1::tails, Nil, a)

    override def +:(a: A): Cons[A] = Cons(a, head0, head1::tails)

    override def headTailNonEmpty: (A, ConsOrUnity[A]) = {
      (head0,
       consOrUnity(head1, tails)
      )
    }

    override def firstLastNonEmpty: (SnocOrUnity[A], A) = snocwise.firstLastNonEmpty

    override def map[B](f: (A) => B): Cons[B] = Cons(f(head0), f(head1), tails map f)

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val h0 = f(zero, head0)
      val h1 = f(h0, head1)
      tails.foldLeft(h1)(f)
    }

    override def renderStructure(sb: StringBuilder): Unit = sb.append("C").append(size)
  }

  final case class Snoc[A](firsts: List[A], last1: A, last0: A) extends SnocOrUnity[A] {

    override def size = firsts.size + 2

    override def conswise: Cons[A] = {
      val h0::h1::t = (last0::last1::firsts).reverse
      Cons(h0, h1, t)
    }

    override def snocwise: Snoc[A] = this

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case Unity(u) =>
        Snoc(last1::firsts, last0, u)
      case r@Cons(_, _, _) =>
        TqA(this +: r +: empty[NonEmpty[A]]) // choice to bias to left
      case Snoc(f, l1, l0) =>
        TqAS(empty :+ this, l1::f, l0)
      case TqCA(h, t, a) =>
        TqA(this +: consOrUnity(h, t) +: a)
      case TqCAS(h, t, a, f, l) =>
        TqAS(this +: consOrUnity(h, t) +: a, f, l)
      case TqCS(h, t, f, l) =>
        TqAS(this +: consOrUnity(h, t) +: empty[NonEmpty[A]], f, l)
      case TqA(a) =>
        TqA(this +: a)
      case TqAS(a, f, l) =>
        TqAS(this +: a, f, l)
    }

    override def :+(a: A): Snoc[A] = Snoc(last1::firsts, last0, a)

    override def +:(a: A): TqCS[A] = TqCS(a, Nil, last1::firsts, last0)

    override def headTailNonEmpty: (A, ConsOrUnity[A]) = conswise.headTailNonEmpty

    override def firstLastNonEmpty: (SnocOrUnity[A], A) = {
      (snocOrUnity(firsts, last1),
        last0
      )
    }

    override def map[B](f: (A) => B): Snoc[B] = Snoc(firsts map f, f(last1), f(last0))

    override def fold[B](zero: B)(f: (B, A) => B): B = {
      val ff = firsts.foldLeft(zero)(f)
      val l1 = f(ff, last1)
      f(l1, last0)
    }

    override def renderStructure(sb: StringBuilder): Unit = sb.append("S").append(size)
  }

  final case class TqCA[A](head: A, tails: List[A], appends: NonEmpty[NonEmpty[A]]) extends NonEmpty[A] {

    override def size: Int = sizeAppends(appends) + 1 + tails.size

    override def conswise: TqCA[A] = TqCA(head, tails, appends.conswise)

    override def snocwise: TqA[A] = TqA(consOrUnity(head, tails).snocwise +: appends.snocwise)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case Unity(u) =>
        TqCAS(head, tails, appends, Nil, u)
      case r@Cons(_, _, _) =>
        TqCA(head, tails, appends :+ r)
      case Snoc(f, l1, l0) =>
        TqCAS(head, tails, appends, l1::f, l0)
      case TqCA(hr, tr, ar) =>
        TqCA(head, tails, appends +:+ (consOrUnity(hr, tr) +: ar))
      case TqCAS(h, t, ar, f, l) =>
        TqCAS(head, tails, appends +:+ (consOrUnity(h, t) +: ar), f, l)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(head, tails, appends :+ consOrUnity(hr, tr), fr, lr)
      case TqA(ra) =>
        TqCA(head, tails, appends +:+ ra)
      case TqAS(ra, f, l) =>
        TqCAS(head, tails, appends +:+ ra, f, l)
    }

    override def :+(a: A): TqCAS[A] = TqCAS(head, tails, appends, Nil, a)

    override def +:(a: A): TqCA[A] = TqCA(a, head::tails, appends)

    override def headTailNonEmpty: (A, TurtleQS3[A]) = tails match {
      case Nil =>
        (head, TqA(appends))
      case h::t =>
        (head, TqCA(h, t, appends))
    }

    override def firstLastNonEmpty: (TurtleQS3[A], A) = appends.firstLastNonEmpty match {
      case (TNil(), al) =>
        al.firstLastNonEmpty match {
          case (TNil(), l) =>
            (consOrUnity(head, tails), l)
          case (f : NonEmpty[A], l) =>
            (consOrUnity(head, tails) +:+ f, l)
        }
      case (af: NonEmpty[NonEmpty[A]], al) =>
        al.firstLastNonEmpty match {
          case (TNil(), l) =>
            (TqCA(head, tails, af), l)
          case (f@Cons(_, _, _), l) =>
            val Snoc(f0, l1, l0) = f.snocwise
            (TqCAS(head, tails, af, l1::f0, l0), l)
          case (Snoc(f, l1, l0), l) =>
            (TqCAS(head, tails, af, l1::f, l0), l)
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

    override def conswise: TqCA[A] = TqCA(head, tails, appends.conswise :+ snocOrUnity(firsts, last).conswise)

    override def snocwise: TqAS[A] = TqAS(consOrUnity(head, tails).snocwise +: appends.snocwise, firsts, last)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case Unity(u) =>
        TqCAS(head, tails, appends, last::firsts, u)
      case r@Cons(_, _, _) =>
        TqCA(head, tails, appends :+ snocOrUnity(firsts, last) :+ r)
      case Snoc(f, l1, l0) =>
        TqCAS(head, tails, appends :+ snocOrUnity(firsts, last), l1::f, l0)
      case TqCA(ch, ct, ar) =>
        TqCA(head, tails, (appends :+ snocOrUnity(firsts, last)) +:+ (consOrUnity(ch, ct) +: ar))
      case TqCAS(h, t, ar, f, l) =>
        TqCAS(head, tails, (appends :+ snocOrUnity(firsts, last)) +:+ (consOrUnity(h, t) +: ar), f, l)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(head, tails, appends :+ snocOrUnity(firsts, last) :+ consOrUnity(hr, tr), fr, lr)
      case TqA(ar) =>
        TqCA(head, tails, (appends :+ snocOrUnity(firsts, last)) +:+ ar)
      case TqAS(ar, f, l) =>
        TqCAS(head, tails, (appends :+ snocOrUnity(firsts, last)) +:+ ar, f, l)
    }

    override def :+(a: A): TqCAS[A] = TqCAS(head, tails, appends, last::firsts, a)

    override def +:(a: A): TqCAS[A] = TqCAS(a, head::tails, appends, firsts, last)

    override def headTailNonEmpty: (A, TurtleQS3[A]) = tails match {
      case Nil =>
        (head, TqAS(appends, firsts, last))
      case h::t =>
        (head, TqCAS(h, t, appends, firsts, last))
    }

    override def firstLastNonEmpty: (TurtleQS3[A], A) = firsts match {
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

    override def conswise: TqCA[A] = TqCA(head, tails, empty :+ snocOrUnity(firsts, last).conswise)

    override def snocwise: TqAS[A] = TqAS(consOrUnity(head, tails).snocwise +: empty, firsts, last)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case Unity(u) =>
        TqCS(head, tails, last::firsts, u)
      case r@Cons(_, _, _) =>
        TqCA(head, tails, snocOrUnity(firsts, last) +: r +: empty[NonEmpty[A]]) // choice to bias to left
      case Snoc(f, l1, l0) =>
        TqCAS(head, tails, snocOrUnity(firsts, last) +: empty, l1::f, l0)
      case TqCA(hr, tr, ar) =>
        TqCA(head, tails, snocOrUnity(firsts, last) +: consOrUnity(hr, tr) +: ar)
      case TqCAS(hr, tr, ar, f, l) =>
        TqCAS(head, tails, snocOrUnity(firsts, last) +: consOrUnity(hr, tr) +: ar, f, l)
      case TqCS(hr, tr, fr, lr) =>
        TqCAS(head, tails, snocOrUnity(firsts, last) +: consOrUnity(hr, tr) +: empty[NonEmpty[A]], fr, lr)
      case TqA(ar) =>
        TqCA(head, tails, snocOrUnity(firsts, last) +: ar)
      case TqAS(ar, f, l) =>
        TqCAS(head, tails, snocOrUnity(firsts, last) +: ar, f, l)
    }

    override def :+(a: A): TqCS[A] = TqCS(head, tails, last :: firsts, a)

    override def +:(a: A): TqCS[A] = TqCS(a, head :: tails, firsts, last)

    override def headTailNonEmpty: (A, TurtleQS3[A]) = {
      tails match {
        case Nil =>
          (head, snocOrUnity(firsts, last))
        case th::tt =>
          (head, TqCS(th, tt, firsts, last))
      }
    }

    override def firstLastNonEmpty: (TurtleQS3[A], A) = {
      firsts match {
        case Nil =>
          (consOrUnity(head, tails), last)
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
      case Unity(u) =>
        TqAS(appends, Nil, u)
      case r@Cons(_, _, _) =>
        TqA(appends :+ r)
      case Snoc(f, l1, l0) =>
        TqAS(appends, l1::f, l0)
      case TqCA(hr, tr, ar) =>
        TqA((appends :+ consOrUnity(hr, tr)) +:+ ar)
      case TqCAS(hr, tr, ar, fr, lr) =>
        TqAS((appends :+ consOrUnity(hr, tr)) +:+ ar, fr, lr)
      case TqCS(hr, tr, fr, lr) =>
        TqAS(appends :+ consOrUnity(hr, tr), fr, lr)
      case TqA(ar) =>
        TqA(appends +:+ ar)
      case TqAS(ar, fr, lr) =>
        TqAS(appends +:+ ar, fr, lr)
    }

    override def :+(a: A): TqAS[A] = TqAS(appends, Nil, a)

    override def +:(a: A): TqCA[A] = TqCA(a, Nil, appends)

    override def headTailNonEmpty: (A, TurtleQS3[A]) = appends.headTailNonEmpty match {
      case (ah, TNil()) => ah.headTailNonEmpty match {
        case (h, TNil()) =>
          (h, empty)
        case (h, t) =>
          (h, t)
      }
      case (ah, at: NonEmpty[NonEmpty[A]]) => ah.headTailNonEmpty match {
        case (h, TNil()) =>
          (h, TqA(at))
        case (h, Cons(ch0, ch1, ct)) =>
          (h, TqCA(ch0, ch1::ct, at))
        case (h, t@Snoc(_, _, _)) =>
          val Cons(h0, h1, ts) = t.conswise
          (h, TqCA(h0, h1::ts, at))
        case (h, t: NonEmpty[A]) =>
          (h, TqA(t +: at))
      }
    }

    override def firstLastNonEmpty: (TurtleQS3[A], A) = appends.firstLastNonEmpty match {
      case (TNil(), al) => al.firstLastNonEmpty match {
        case (TNil(), l) =>
          (empty, l)
        case (f, l) =>
          (f, l)
      }
      case (af: NonEmpty[NonEmpty[A]], al) => al.firstLastNonEmpty match {
        case (TNil(), l) =>
          (TqA(af), l)
        case (f@Cons(_, _, _), l) =>
          val Snoc(fs, l1, l0) = f.snocwise
          (TqAS(af, l1::fs, l0), l)
        case (Snoc(fs, ls1, ls0), l) =>
          (TqAS(af, ls1::fs, ls0), l)
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

    override def conswise: TqA[A] = TqA(appends.conswise :+ snocOrUnity(firsts, last).conswise)

    override def snocwise: TqAS[A] = TqAS(appends.snocwise, firsts, last)

    override def +:+(rhs: NonEmpty[A]): NonEmpty[A] = rhs match {
      case Unity(u) =>
        TqAS(appends, last::firsts, u)
      case r@Cons(_, _, _) =>
        TqA(appends :+ snocOrUnity(firsts, last) :+ r)
      case Snoc(f, l1, l0) =>
        TqAS(appends :+ snocOrUnity(firsts, last), l1::f, l0)
      case TqCA(hr, tr, ar) =>
        TqA((appends :+ snocOrUnity(firsts, last)) +:+ (consOrUnity(hr, tr) +: ar))
      case TqCAS(hr, tr, ar, fr, lr) =>
        TqAS((appends :+ snocOrUnity(firsts, last)) +:+ (consOrUnity(hr, tr) +: ar), fr, lr)
      case TqCS(hr, tr, fr, lr) =>
        TqAS(appends :+ snocOrUnity(firsts, last) :+ consOrUnity(hr, tr), fr, lr)
      case TqA(ar) =>
        TqA((appends :+ snocOrUnity(firsts, last)) +:+ ar)
      case TqAS(ar, fr, lr) =>
        TqAS((appends :+ snocOrUnity(firsts, last)) +:+ ar, fr, lr)
    }

    override def :+(a: A): TqAS[A] = TqAS(appends, last::firsts, a)

    override def +:(a: A): TqCAS[A] = TqCAS(a, Nil, appends, firsts, last)

    override def headTailNonEmpty: (A, TurtleQS3[A]) = appends.headTailNonEmpty match {
      case (ah, TNil()) =>
        ah.headTailNonEmpty match {
          case (h, TNil()) =>
            (h, snocOrUnity(firsts, last))
          case (h, t) =>
            (h, t ++ snocOrUnity(firsts, last))
        }
      case (ah, at: NonEmpty[NonEmpty[A]]) =>
        ah.headTailNonEmpty match {
          case (h, TNil()) =>
            (h, TqAS(at, firsts, last))
          case (h, Cons(ch0, ch1, ct)) =>
            (h, TqCAS(ch0, ch1::ct, at, firsts, last))
          case (h, t@Snoc(_, _, _)) =>
            val Cons(t0, t1, hs) = t.conswise
            (h, TqCAS(t0, t1::hs, at, firsts, last))
          case (h, t: NonEmpty[A]) =>
            (h, TqAS(t +: at, firsts, last))
        }
    }

    override def firstLastNonEmpty: (TurtleQS3[A], A) = firsts match {
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
