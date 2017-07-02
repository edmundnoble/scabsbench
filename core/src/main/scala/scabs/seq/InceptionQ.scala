package scabs.seq

import shapeless.newtype
import shapeless.newtype._

trait InceptionQ[E] {
  def toStructure: String

  def size: Int

  def isEmpty: Boolean

  def ++(rhs: InceptionQ[E]): InceptionQ[E]

  def :+(a: E): InceptionQ[E]

  def +:(a: E): InceptionQ[E]

  def uncons: Option[(E, InceptionQ[E])]

  def unsnoc: Option[(InceptionQ[E], E)]

  def map[B](f: E => B): InceptionQ[B]

  def fold[B](zero: B)(f: (B, E) => B): B

  def foreach[B](f: E => B): Unit
}

object InceptionQ {

  implicit val sequenceForInceptionQ: Sequence[InceptionQ] = new Sequence[InceptionQ] {
    override def toList[A](q: InceptionQ[A]): List[A] = {
      val builder = List.newBuilder[A]
      q foreach (builder += _)
      builder.result()
    }

    override def empty[A]: InceptionQ[A] = InceptionQ.empty[A]

    override def head[A](queue: InceptionQ[A]): A = uncons(queue).get._1

    override def foreach[A, U](q: InceptionQ[A])(f: (A) => U): Unit = {
      q foreach f
    }

    override def snoc[A](q: InceptionQ[A], y: A): InceptionQ[A] = q :+ y

    override def map[A, B](q: InceptionQ[A])(f: (A) => B): InceptionQ[B] = q map f

    override def cons[A](x: A, q: InceptionQ[A]): InceptionQ[A] = x +: q

    override def lengthSeq[A](q: InceptionQ[A]): Int = q.size

    override def init[A](queue: InceptionQ[A]): InceptionQ[A] = unsnoc(queue).get._1

    override def unsnoc[A](s: InceptionQ[A]): Option[(InceptionQ[A], A)] = s.unsnoc

    override def last[A](queue: InceptionQ[A]): A = unsnoc(queue).get._2

    override def tail[A](queue: InceptionQ[A]): InceptionQ[A] = uncons(queue).get._2

    override def isEmpty[A](q: InceptionQ[A]): Boolean = q.isEmpty

    override def uncons[A](s: InceptionQ[A]): Option[(A, InceptionQ[A])] = try {
      val u = s.uncons
//      println(s"Uncons:\n\t$s${u match {
//        case None => ""
//        case Some((a, i)) => s"\n\t$a\n\t$i"
//      }}")
      u
    } catch {
      case e : StackOverflowError =>
        println(s"Uncons failed")
//        println(s"Uncons failed for: $s")
        throw e
    }

    override def concat[A](fst: InceptionQ[A], snd: InceptionQ[A]): InceptionQ[A] = try {
      val c = fst ++ snd
//      println(s"q2++:\n\t${fst.toStructure}\n\t${snd.toStructure}\n\t${c.toStructure}")
      c
    } catch {
      case e : StackOverflowError =>
        println(s"concat failed")
//        println(s"concat failed for:\n\t$fst\n\t$snd")
        throw e
    }

//    override def fold[A, B](q: InceptionQ[A])(z: B)(f: (B, A) => B): B = /*try*/ {
//      val b = q.fold(z)(f)
//      println(s"fold: ${q.toStructure}")
//      println(s"fold q2: ${q.size}")
//      b
//    } catch {
//      case e : StackOverflowError =>
//        println(s"Fold failed")
//        println(s"Fold failed for: $q")
//        throw e
//    }
    override def cata[A, B](q: InceptionQ[A])(z: B)(f: (B, A) => B): B = {
      var result = z
      this.foreach(q)(x => result = f(result, x))
//      println(s"fold q2: ${q.toStructure}")
      result
    }



    override def toSeq[A](xs: List[A]): InceptionQ[A] = xs match {
      case Nil =>
        empty
      case u::Nil =>
        Unity(u)
      case c::cs =>
        Cons(ConsList(c, cs))
    }
  }
  // singleton hack
  private val emptyNothing = QNil()
  def empty[E]: InceptionQ[E] = emptyNothing.asInstanceOf[InceptionQ[E]]

  final case class Nel[+E](head: E, tail: List[E]) {
    def size: Int = 1 + tail.size
    def foldLeft[B](zero: B)(f: (B, E) => B): B = tail.foldLeft(f(zero, head))(f)
    def map[B](f: E => B): Nel[B] = Nel(f(head), tail map f)
    def foreach[B](f: E => B): Unit = {
      f(head)
      tail foreach f
    }
    def reverse: Nel[E] = (head::tail).reverse match {
      case h::t => Nel(h, t)
    }
    def mkString(start: String, sep: String, end: String) = (head::tail).mkString(start, sep, end)
  }
  object Nel {
    def apply[E](e: E): Nel[E] = Nel(e, Nil)
    def apply[E](e: E, es: Nel[E]): Nel[E] = Nel(e, es.head::es.tail)
  }

  type ConsList[E] = Newtype[Nel[E], ConsListOps[E]]
  class ConsListOps[E](val _l: Nel[E]) extends AnyVal {
    def +:(e: E): ConsList[E] = ConsList(e, _l)
    def size: Int = _l.size
    def fold[B](zero: B)(f: (B, E) => B): B = _l.foldLeft(zero)(f)
    def map[B](f: E => B): ConsList[B] = ConsList(_l map f)
    def foreach[B](f: E => B): Unit = _l foreach f
    def reverse: SnocList[E] = SnocList(_l.reverse)
  }
  implicit def consListOps[E](l: Nel[E]): ConsListOps[E] = new ConsListOps(l)
  object ConsList {
    def apply[A](as: Nel[A]): ConsList[A] = newtype(as)
    def apply[A](a: A, as: List[A]): ConsList[A] = newtype(Nel(a, as))
    def apply[A](a: A, as: Nel[A]): ConsList[A] = newtype(Nel(a, as.head::as.tail))
  }

  type ConsCarryList[E] = Newtype[Nel[ConsCarry[E]], ConsCarryListOps[E]]
  class ConsCarryListOps[E](val _l: Nel[ConsCarry[E]]) extends AnyVal {
    def +: (c: ConsCarry[E]): ConsCarryList[E] = ConsCarryList(c, _l)
    def carrySize[C]: Int = _l.foldLeft(0)((a, f) => a + f.size)
    def foldCarry[B](zero: B)(f: (B, E) => B): B =
      _l.foldLeft(zero)((b, c) => c.fold(b)(f))
    def foreachCarry[B](f: E => B) = _l.foreach(_ foreach f)
    def mapCarry[B](f: E => B): ConsCarryList[B] =
      ConsCarryList(_l map (_ map f))

    def nextConsBlock: (ConsList[E], List[ConsCarry[E]]) = _l match {
      case Nel(h, tl) =>
        h match {
          case Cons(c) => c -> tl
          case ConsListCarry(cs) =>
            cs.nextConsBlock match {
              case (cl, cc) =>
                cl -> ConsCarry(cc, tl)
            }
          case ReverseS(s) =>
            s.reverse.nextConsBlock match {
              case (cl, cc) =>
                cl -> ConsCarry(cc, tl)
            }
          case ReverseSnoc(cs) =>
            cs.reverse.nextConsBlock match {
              case (cl, cc) =>
                cl -> ConsCarry(cc, tl)
            }
        }
    }

    def reverse: SnocCarryList[E] = SnocCarryList(_l.reverse map (_.reverse))

    def toStructure: String = _l.map(_.toStructure).mkString("<", ",", ">")
  }

  implicit def consCarryListOps[E](l: Nel[ConsCarry[E]]): ConsCarryListOps[E] = new ConsCarryListOps(l)
  object ConsCarryList {
    def apply[A](as: Nel[ConsCarry[A]]): ConsCarryList[A] = newtype(as)
    def apply[A](a: ConsCarry[A], as: List[ConsCarry[A]] = Nil): ConsCarryList[A] = newtype(Nel(a, as))
    def apply[A](a: ConsCarry[A], as: Nel[ConsCarry[A]]): ConsCarryList[A] = newtype(Nel(a, as.head::as.tail))
  }

  type SnocList[E] = Newtype[Nel[E], SnocListOps[E]]
  class SnocListOps[E](val _l: Nel[E]) extends AnyVal {
    def :+(e: E): SnocList[E] = SnocList(e, _l)

    def size: Int = _l.size

    def fold[B](zero: B)(f: (B, E) => B): B = _l.foldLeft(zero)(f)

    def map[B](f: E => B): SnocList[B] = SnocList(_l map f)

    def foreach[B](f: E => B): Unit = _l foreach f

    def reverse: ConsList[E] = ConsList(_l.reverse)
  }
  implicit def snocListOps[E](l: Nel[E]): SnocListOps[E] = new SnocListOps(l)
  object SnocList {
    def apply[A](a: A, as: Nel[A]): SnocList[A] = newtype(Nel(a, as.head::as.tail))
    def apply[A](a: A, as: List[A]): SnocList[A] = newtype(Nel(a, as))
    def apply[A](as: Nel[A]): SnocList[A] = newtype(as)
  }

  type SnocCarryList[E] = Newtype[Nel[SnocCarry[E]], SnocCarryListOps[E]]
  class SnocCarryListOps[E](val _l: Nel[SnocCarry[E]]) extends AnyVal {
    def :+ (e: SnocCarry[E]): SnocCarryList[E] = SnocCarryList(_l, e)

    def :<: (e: SnocCarry[E]): SnocCarryList[E] = SNil :+ e :+ {
      _l match {
        case Nel(h, Nil) =>
          h
        case _ =>
          SnocListCarry(SnocCarryList(_l))
      }
    }

    def carrySize: Int = _l.foldLeft(0)((a, f) => a + f.size)

    def foldCarry[B](zero: B)(f: (B, E) => B): B =
      _l.foldLeft(zero)((b, c) => c.fold(b)(f))

    def mapCarry[B](f: E => B): SnocCarryList[B] =
      SnocCarryList(_l.map(_ map f))

    def foreachCarry[B](f: E => B): Unit = _l foreach (_ foreach f)

    def reverse: ConsCarryList[E] = ConsCarryList(_l.reverse map (_.reverse))

    def nextSnocBlock: (List[SnocCarry[E]], SnocList[E]) = _l match {
      case Nel(h, tl) =>
        h match {
          case Snoc(s) => tl -> s
          case SnocListCarry(cs) =>
            cs.nextSnocBlock match {
              case (ss, sl) =>
                SnocCarry(tl, ss) -> sl
            }
          case ReverseC(c) =>
            c.reverse.nextSnocBlock match {
              case (ss, sl) =>
                SnocCarry(tl, ss) -> sl
            }
          case ReverseCons(cs) =>
            cs.reverse.nextSnocBlock match {
              case (ss, sl) =>
                SnocCarry(tl, ss) -> sl
            }
        }
    }

    def toStructure: String = _l.map(_.toStructure).mkString("<", ",", ">")
  }
  implicit def snocCarryListOps[E](l: Nel[SnocCarry[E]]): SnocCarryListOps[E] = new SnocCarryListOps(l)
  object SnocCarryList {
    def apply[A](as: Nel[SnocCarry[A]]): SnocCarryList[A] = newtype(as)
    def apply[A](as: List[SnocCarry[A]], a: SnocCarry[A]): SnocCarryList[A] = newtype(Nel(a, as))
    def apply[A](a: SnocCarry[A]): SnocCarryList[A] = newtype(Nel(a))
    def apply[A](as: Nel[SnocCarry[A]], a: SnocCarry[A]): SnocCarryList[A] = newtype(Nel(a, as.head::as.tail))
  }

  object CNil {
    def +:[E](c: ConsCarry[E]): ConsCarryList[E] = ConsCarryList(c)
    def +:[E](e: E): ConsList[E] = ConsList(Nel(e))
  }

  object SNil {
    def :+ [E](c: SnocCarry[E]): SnocCarryList[E] = SnocCarryList(c)
    def :+ [E](e: E): SnocList[E] = SnocList(Nel(e))
  }

  sealed trait Carry[E] {

    def toStructure: String

    def size: Int
    def fold[B](zero: B)(f: (B, E) => B): B
  }

  sealed trait ConsCarry[E] extends Carry[E] {
    final def :>: (l: ConsCarryList[E]): ConsCarryList[E] = {
      l._l match {
        case Nel(h, Nil) =>
          h
        case _ =>
          ConsListCarry(l)
      }
    } +: this +: CNil

    def map[B](f: E => B): ConsCarry[B]

    def foreach[B](f: E => B): Unit

    def reverse: SnocCarry[E]

    def nextConsBlock: (ConsList[E], List[ConsCarry[E]])
  }
  final object ConsCarry {
    def apply[E](cc: List[ConsCarry[E]], tl: List[ConsCarry[E]]): List[ConsCarry[E]] = (cc, tl) match {
      case (Nil, _) =>
        tl
      case (_, Nil) =>
        cc
      case (h::Nil, _) =>
        h :: tl
      case (h::t, _) =>
        ConsListCarry(ConsCarryList(Nel(h, t))) :: tl
    }
  }


  sealed trait SnocCarry[E] extends Carry[E] {
    def map[B](f: E => B): SnocCarry[B]

    def foreach[B](f: E => B): Unit

    def reverse: ConsCarry[E]

    def nextSnocBlock: (List[SnocCarry[E]], SnocList[E])
  }

  final object SnocCarry {
    def apply[E](tl: List[SnocCarry[E]], cc: List[SnocCarry[E]]): List[SnocCarry[E]] = (tl, cc) match {
      case (_, Nil) =>
        tl
      case (Nil, _) =>
        cc
      case (_, h::Nil) =>
        h :: tl
      case (_, h::t) =>
        SnocListCarry(SnocCarryList(Nel(h, t))) :: tl
    }
  }

  final case class ConsListCarry[E](cs: ConsCarryList[E]) extends ConsCarry[E] {

    override def toStructure: String = cs.toStructure

    override def map[B](f: (E) => B): ConsListCarry[B] = ConsListCarry(cs mapCarry f)

    override def foreach[B](f: (E) => B): Unit = cs foreachCarry f

    override def size: Int = cs.carrySize

    override def fold[B](zero: B)(f: (B, E) => B): B = cs.foldCarry(zero)(f)

    def reverse: SnocListCarry[E] = SnocListCarry(SnocCarryList(cs._l.reverse.map(_.reverse)))

    override def nextConsBlock: (ConsList[E], List[ConsCarry[E]]) = cs.nextConsBlock
  }

  final case class SnocListCarry[E](cs: SnocCarryList[E]) extends SnocCarry[E] {

    override def toStructure: String = cs.toStructure

    override def map[B](f: (E) => B): SnocCarry[B] = SnocListCarry(cs mapCarry f)

    override def foreach[B](f: (E) => B): Unit = cs foreachCarry f

    override def size: Int = cs.carrySize

    override def fold[B](zero: B)(f: (B, E) => B): B = cs.foldCarry(zero)(f)

    override def reverse: ConsCarry[E] = ConsListCarry(ConsCarryList(cs._l.reverse.map(_.reverse)))

    override def nextSnocBlock: (List[SnocCarry[E]], SnocList[E]) = cs.nextSnocBlock
  }

  final case class ReverseS[E](c: SnocCarry[E]) extends ConsCarry[E] {


    override def toStructure: String = s"R(${c.toStructure})"

    override def size: Int = c.size

    override def map[B](f: (E) => B): ConsCarry[B] = ReverseS(c map f)

    override def foreach[B](f: E => B): Unit = c foreach f

    override def fold[B](zero: B)(f: (B, E) => B): B = c.fold(zero)(f)

    override def reverse: SnocCarry[E] = c

    override def nextConsBlock: (ConsList[E], List[ConsCarry[E]]) = c.reverse.nextConsBlock
  }

  final case class ReverseSnoc[E](cs: SnocCarryList[E]) extends ConsCarry[E] {

    override def toStructure: String = s"R(${cs.toStructure})"

    override def size: Int = cs.carrySize

    override def map[B](f: (E) => B): ConsCarry[B] = ReverseSnoc(cs mapCarry f)

    override def foreach[B](f: (E) => B): Unit = cs foreachCarry f

    override def fold[B](zero: B)(f: (B, E) => B): B = cs.foldCarry(zero)(f)

    override def reverse: SnocCarry[E] = SnocListCarry(cs)

    override def nextConsBlock: (ConsList[E], List[ConsCarry[E]]) = cs.reverse.nextConsBlock
  }

  final case class ReverseC[E](c: ConsCarry[E]) extends SnocCarry[E] {

    override def toStructure: String = s"R(${c.toStructure})"

    override def size: Int = c.size

    override def map[B](f: (E) => B): SnocCarry[B] = ReverseC(c map f)


    override def foreach[B](f: (E) => B): Unit = c foreach f

    override def fold[B](zero: B)(f: (B, E) => B): B = c.fold(zero)(f)

    override def reverse: ConsCarry[E] = c

    override def nextSnocBlock: (List[SnocCarry[E]], SnocList[E]) = c.reverse.nextSnocBlock
  }

  final case class ReverseCons[E](cs: ConsCarryList[E]) extends SnocCarry[E] {

    override def toStructure: String = s"R(${cs.toStructure})"

    override def size: Int = cs.carrySize

    override def map[B](f: (E) => B): SnocCarry[B] = ReverseCons(cs mapCarry f)


    override def foreach[B](f: (E) => B): Unit = cs foreachCarry f

    override def fold[B](zero: B)(f: (B, E) => B): B = cs.foldCarry(zero)(f)

    override def reverse: ConsCarry[E] = ConsListCarry(cs)

    override def nextSnocBlock: (List[SnocCarry[E]], SnocList[E]) = cs.reverse.nextSnocBlock
  }

  def rev[E](s: SnocCarry[E]): ConsCarry[E] = ReverseS(s)
  def rev[E](c: ConsCarry[E]): SnocCarry[E] = ReverseC(c)
  def rev[E](l: ConsCarryList[E]): SnocCarry[E] = ReverseCons(l)
  def rev[E](l: SnocCarryList[E]): ConsCarry[E] = ReverseSnoc(l)


  case class QNil[E]() extends InceptionQ[E] {

    override def toStructure: String = "QNil(0)"

    override def size: Int = 0

    override def isEmpty: Boolean = true

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs

    override def :+(a: E): InceptionQ[E] = Unity(a)

    override def +:(a: E): InceptionQ[E] = Unity(a)

    override def uncons: Option[(E, InceptionQ[E])] = None

    override def unsnoc: Option[(InceptionQ[E], E)] = None

    override def map[B](f: E => B): InceptionQ[B] = empty[B]

    override def fold[B](zero: B)(f: (B, E) => B): B = zero

    override def foreach[B](f: (E) => B): Unit = {}
  }

  final case class Unity[E](u: E) extends InceptionQ[E] /* with SnocCarry[E] with ConsCarry[E] */ {

    override def toStructure: String = "U(1)"

    override def size: Int = 1

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => Cons      (u +: s +: CNil      )
      case Cons      (c         ) => Cons      (u +: c              )
      case ConsP     (c, p      ) => ConsP     (u +:    c, p        )
      case ConsA     (c,    a   ) => ConsA     (u +:    c,    a     )
      case ConsPA    (c, p, a   ) => ConsPA    (u +:    c, p, a     )
      case ConsSnoc  (c,       s) => ConsSnoc  (u +:    c,         s)
      case ConsPSnoc (c, p   , s) => ConsPSnoc (u +:    c, p,      s)
      case ConsASnoc (c,    a, s) => ConsASnoc (u +:    c,    a,   s)
      case ConsPASnoc(c, p, a, s) => ConsPASnoc(u +:    c, p, a,   s)
      case QNil()                 => this
      case P         (   p      ) => ConsP     (u +: CNil, p        )
      case A         (      a   ) => ConsA     (u +: CNil,    a     )
      case PA        (   p, a   ) => ConsPA    (u +: CNil, p, a     )
      case Snoc      (         s) => ConsSnoc  (u +: CNil,         s)
      case PSnoc     (   p,    s) => ConsPSnoc (u +: CNil, p,      s)
      case ASnoc     (      a, s) => ConsASnoc (u +: CNil,    a,   s)
      case PASnoc    (   p, a, s) => ConsPASnoc(u +: CNil, p, a,   s)
    }

    override def :+(a: E): InceptionQ[E] = Snoc(SNil :+ u :+ a)

    override def +:(a: E): InceptionQ[E] = Cons(a +: u +: CNil)

    override def uncons: Option[(E, InceptionQ[E])] = Some(u -> empty[E])
//
    override def unsnoc: Option[(InceptionQ[E], E)] = Some(empty[E] -> u)

    override def map[B](f: (E) => B): Unity[B] = Unity(f(u))

    override def fold[B](zero: B)(f: (B, E) => B): B = f(zero, u)


//    override def reverse: Unity[E] = this
    override def foreach[B](f: (E) => B): Unit = f(u)
  }

  final case class Cons[E](cons: ConsList[E]) extends InceptionQ[E] with ConsCarry[E] {

    override def toStructure: String = s"Cons(${cons.size},_,_,_)"

    override def size: Int = cons.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
        case Unity     (         s) => ConsSnoc  (cons,             SNil :+ s)
        case c@Cons    (_         ) => ConsP     (cons,      c  +: CNil      )
        case ConsP     (c, p      ) => ConsP     (cons, Cons(c) +:    p      )
        case ConsA     (c,    a   ) => ConsPA    (cons, Cons(c) +: CNil, a   )
        case ConsPA    (c, p, a   ) => ConsPA    (cons, Cons(c) +:    p, a   )
        case ConsSnoc  (c,       s) => ConsPSnoc (cons, Cons(c) +: CNil,    s)
        case ConsPSnoc (c, p,    s) => ConsPSnoc (cons, Cons(c) +:    p,    s)
        case ConsASnoc (c,    a, s) => ConsPASnoc(cons, Cons(c) +: CNil, a, s)
        case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, Cons(c) +:    p, a, s)
        case QNil()                 => this
        case P         (   p      ) => ConsP     (cons,               p      )
        case A         (      a   ) => ConsA     (cons,                  a   )
        case PA        (   p, a   ) => ConsPA    (cons,               p, a   )
        case Snoc      (         s) => ConsSnoc  (cons,                     s)
        case PSnoc     (   p,    s) => ConsPSnoc (cons,               p,    s)
        case ASnoc     (      a, s) => ConsASnoc (cons,                  a, s)
        case PASnoc    (   p, a, s) => ConsPASnoc(cons,               p, a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsSnoc(cons, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = Cons(a +: cons)

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) => h -> empty[E]
        case Nel(h, t::ts) => h -> Cons(newtype(Nel(t, ts)))
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = reverse.unsnoc

    override def map[B](f: (E) => B): Cons[B] = Cons(cons map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
//      println("*** folding Cons")
      cons.fold(zero)(f)
    }

    override def reverse: Snoc[E] = Snoc(cons.reverse)

    override def nextConsBlock: (ConsList[E], List[ConsCarry[E]]) = (cons, Nil)

    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
    }
  }

  final case class ConsP[E](cons: ConsList[E], pre: ConsCarryList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ConsP(${cons.size},${pre.toStructure},_,_}"

    override def size: Int = cons.size + pre.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
        case Unity     (         s) => ConsPSnoc (cons, pre,                          SNil :+ s)
        case c@Cons    (_         ) => ConsPA    (cons, pre, SNil :+ rev(     c      )         )
        case ConsP     (c, p      ) => ConsPA    (cons, pre, SNil :+ rev(Cons(c) +: p)         )
        case ConsA     (c,    a   ) => ConsPA    (cons, pre,         rev(Cons(c)     ) :<: a   )
        case ConsPA    (c, p, a   ) => ConsPA    (cons, pre,         rev(Cons(c) +: p) :<: a   )
        case ConsSnoc  (c,       s) => ConsPASnoc(cons, pre, SNil :+ rev(Cons(c)     ),      s)
        case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, pre, SNil :+ rev(Cons(c) +: p)      , s)
        case ConsASnoc (c,    a, s) => ConsPASnoc(cons, pre,         rev(Cons(c)     ) :<: a, s)
        case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, pre,         rev(Cons(c) +: p) :<: a, s)
        case QNil()                 => this
        case P         (   p      ) => ConsPA    (cons, pre, SNil :+ rev(           p)         )
        case A         (      a   ) => ConsPA    (cons, pre,                               a   )
        case PA        (   p, a   ) => ConsPA    (cons, pre,         rev(           p) :<: a   )
        case Snoc      (         s) => ConsPSnoc (cons, pre,                                  s)
        case PSnoc     (   p,    s) => ConsPASnoc(cons, pre, SNil :+ rev(           p)      , s)
        case ASnoc     (      a, s) => ConsPASnoc(cons, pre,                               a, s)
        case PASnoc    (   p, a, s) => ConsPASnoc(cons, pre,         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsPSnoc(cons, pre, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsP(a +: cons, pre)

    override def map[B](f: (E) => B): InceptionQ[B] = ConsP(cons map f, pre mapCarry f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val c = cons.fold(zero)(f)
      val p = pre.foldCarry(c)(f)
      p
    }

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) =>
          pre.nextConsBlock match {
            case (cns, Nil) =>
              h -> Cons(cns)
            case (cns, ph::pt) =>
              h -> ConsP(cns, ConsCarryList(Nel(ph, pt)))
          }
        case Nel(h, t::ts) =>
          h -> ConsP(ConsList(Nel(t, ts)), pre)
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = pre.reverse.nextSnocBlock match {
      case (Nil, snc) =>
        ConsSnoc(cons, snc).unsnoc
      case (ah::at, snc) =>
        ConsASnoc(cons, SnocCarryList(Nel(ah, at)), snc).unsnoc
    }

    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
      pre foreachCarry f
    }
  }

  final case class ConsA[E](cons: ConsList[E], app: SnocCarryList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ConsA(${cons.size},_,${app.toStructure},_)"

    override def size: Int = cons.size + app.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
        case Unity     (         s) => ConsASnoc (cons,                   app,                     SNil :+ s)
        case c@Cons    (_         ) => ConsA     (cons,                   app  :+ rev(     c      )         )
        case ConsP     (c, p      ) => ConsPA    (cons, rev(app) +: CNil, SNil :+ rev(Cons(c) +: p)         )
        case ConsA     (c,    a   ) => ConsPA    (cons, rev(app) +: CNil,         rev(Cons(c)     ) :<: a   )
        case ConsPA    (c, p, a   ) => ConsPA    (cons, rev(app) +: CNil,         rev(Cons(c) +: p) :<: a   )
        case ConsSnoc  (c,       s) => ConsASnoc (cons,                   app  :+ rev(Cons(c)     )      , s)
        case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, rev(app) +: CNil, SNil :+ rev(Cons(c) +: p)      , s)
        case ConsASnoc (c,    a, s) => ConsPASnoc(cons, rev(app) +: CNil,         rev(Cons(c)     ) :<: a, s)
        case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, rev(app) +: CNil,         rev(Cons(c) +: p) :<: a, s)
        case QNil()                 => this
        case P         (   p      ) => ConsA     (cons,                   app  :+ rev(           p)         )
        case A         (      a   ) => ConsPA    (cons, rev(app) +: CNil,                               a   )
        case PA        (   p, a   ) => ConsPA    (cons, rev(app) +: CNil,         rev(           p) :<: a   )
        case Snoc      (         s) => ConsASnoc (cons,                   app,                             s)
        case PSnoc     (   p,    s) => ConsASnoc (cons,                   app  :+ rev(           p)      , s)
        case ASnoc     (      a, s) => ConsPASnoc(cons, rev(app) +: CNil,                               a, s)
        case PASnoc    (   p, a, s) => ConsPASnoc(cons, rev(app) +: CNil,         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsASnoc(cons, app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsA(a +: cons, app)

    override def map[B](f: (E) => B): InceptionQ[B] = ConsA(cons map f, app mapCarry f)


    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
      app foreachCarry f
    }

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val c = cons.fold(zero)(f)
      val a = app.foldCarry(c)(f)
      a
    }

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) =>
          app.reverse.nextConsBlock match {
            case (cns, Nil) =>
              h -> Cons(cns)
            case (cns, ph::pt) =>
              h -> ConsP(cns, ConsCarryList(Nel(ph, pt)))
          }
        case Nel(h, ch::ct) =>
          h -> ConsA(ConsList(Nel(ch, ct)), app)
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = app.nextSnocBlock match {
      case (Nil, snc) =>
        ConsSnoc(cons, snc).unsnoc
      case (ah::at, snc) =>
        ConsASnoc(cons, SnocCarryList(Nel(ah, at)), snc).unsnoc
    }

  }

  final case class ConsPA[E](cons: ConsList[E], pre: ConsCarryList[E], app: SnocCarryList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ConsPA(${cons.size},${pre.toStructure},${app.toStructure},_)"

    override def size: Int = cons.size + pre.carrySize + app.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsPASnoc(cons, pre                   , app                    , SNil :+ s)
      case c@Cons    (_         ) => ConsPA    (cons, pre, app :+ rev(     c      )         )
      case ConsP     (c, p      ) => ConsPA    (cons, pre, app :+ rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => ConsPA    (cons, pre             ,         rev(Cons(c)     ) :<: a   )
      case ConsPA    (c, p, a   ) => ConsPA    (cons, pre :>: rev(app),         rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => ConsPASnoc(cons, pre :>: rev(app), SNil :+ rev(Cons(c) )          , s)
      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, pre :>: rev(app), SNil :+ rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, pre :>: rev(app),         rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, pre :>: rev(app),         rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => ConsPA    (cons, pre :>: rev(app), SNil :+ rev(           p)         )
      case A         (      a   ) => ConsPA    (cons, pre :>: rev(app),                               a   )
      case PA        (   p, a   ) => ConsPA    (cons, pre :>: rev(app),         rev(           p) :<: a   )
      case Snoc      (         s) => ConsPASnoc(cons, pre             ,                             app, s)
      case PSnoc     (   p,    s) => ConsPASnoc(cons, pre             , app  :+ rev(           p)      , s)
      case ASnoc     (      a, s) => ConsPASnoc(cons, pre :>: rev(app),                               a, s)
      case PASnoc    (   p, a, s) => ConsPASnoc(cons, pre :>: rev(app),         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsPASnoc(cons, pre, app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPA(a +: cons, pre, app)

    override def map[B](f: (E) => B): InceptionQ[B] = ConsPA(cons map f, pre mapCarry f, app mapCarry f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val c = cons.fold(zero)(f)
      val p = pre.foldCarry(c)(f)
      val a = app.foldCarry(p)(f)
      a
    }

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) =>
          h -> {
            pre.nextConsBlock match {
              case (cns, Nil) =>
                ConsA(cns, app)
              case (cns, ph::pt) =>
                ConsPA(cns, ConsCarryList(Nel(ph, pt)), app)
            }
          }
        case Nel(h, ch::ct) =>
          h -> ConsPA(ConsList(Nel(ch, ct)), pre, app)
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = app.nextSnocBlock match {
      case (Nil, snc) =>
        ConsPSnoc(cons, pre, snc).unsnoc
      case (ah::at, snc) =>
        ConsPASnoc(cons, pre, SnocCarryList(Nel(ah, at)), snc).unsnoc
    }

    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
      pre foreachCarry f
      app foreachCarry f
    }
  }

  final case class ConsSnoc[E](cons: ConsList[E], snoc: SnocList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ConsSnoc(${cons.size},_,_,${snoc.size}"

    override def size: Int = cons.size + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
        case Unity     (         s) => ConsASnoc (cons,                          SNil :+ SnoC(snoc),                     SNil :+ s)
        case c@Cons    (_         ) => ConsA     (cons,                          SNil :+ SnoC(snoc) :+  rev(     c      )         )
        case ConsP     (c, p      ) => ConsA     (cons,                          SNil :+ SnoC(snoc) :+  rev(Cons(c) +: p)         )
        case ConsA     (c,    a   ) => ConsPA    (cons, rev(SnoC(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a   )
        case ConsPA    (c, p, a   ) => ConsPA    (cons, rev(SnoC(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a   )
        case ConsSnoc  (c,       s) => ConsASnoc (cons,                          SNil :+ SnoC(snoc) :+  rev(Cons(c)     )      , s)
        case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, rev(SnoC(snoc)) +: CNil, SNil               :+  rev(Cons(c) +: p)      , s)
        case ConsASnoc (c,    a, s) => ConsPASnoc(cons, rev(SnoC(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a, s)
        case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, rev(SnoC(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a, s)
        case QNil()                 => this
        case P         (   p      ) => ConsA     (cons,                          SNil :+ SnoC(snoc) :+  rev(           p)         )
        case A         (      a   ) => ConsA     (cons,                                  SnoC(snoc)                       :<: a   )
        case PA        (   p, a   ) => ConsPA    (cons, rev(SnoC(snoc)) +: CNil,                        rev(           p) :<: a   )
        case Snoc      (         s) => ConsASnoc (cons,                          SNil :+ SnoC(snoc)                            , s)
        case PSnoc     (   p,    s) => ConsASnoc (cons,                          SNil :+ SnoC(snoc) :+  rev(           p)      , s)
        case ASnoc     (      a, s) => ConsASnoc (cons,                                  SnoC(snoc)                       :<: a, s)
        case PASnoc    (   p, a, s) => ConsPASnoc(cons, rev(SnoC(snoc)) +: CNil,                        rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsSnoc(cons, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsSnoc(a +: cons, snoc)

    override def map[B](f: (E) => B): InceptionQ[B] = ConsSnoc(cons map f, snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val c = cons.fold(zero)(f)
      val s = snoc.fold(c)(f)
      s
    }

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) =>
          h -> Cons(snoc.reverse)
        case Nel(h, ch::ct) =>
          h -> ConsSnoc(ConsList(Nel(ch, ct)), snoc)
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) =>
          Snoc(cons.reverse) -> h
        case Nel(h, sh::st) =>
          ConsSnoc(cons, SnocList(Nel(sh, st))) -> h
      }
    }

    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
      snoc foreach f
    }
  }

  final case class ConsPSnoc[E](cons: ConsList[E], pre: ConsCarryList[E], snoc: SnocList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ConsPSnoc(${cons.size},${pre.toStructure},_,${snoc.size})"

    override def size: Int = cons.size + pre.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsPASnoc(cons, pre                    , SNil :+ SnoC(snoc),                    SNil :+ s)
      case Cons      (c         ) => ConsPA    (cons, pre                    , SNil :+ SnoC(snoc) :+ rev(Cons(c)     )         )
      case ConsP     (c, p      ) => ConsPA    (cons, pre                    , SNil :+ SnoC(snoc) :+ rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => ConsPA    (cons, pre :>: rev(SnoC(snoc)),                       rev(Cons(c)     ) :<: a   )
      case ConsPA    (c, p, a   ) => ConsPA    (cons, pre :>: rev(SnoC(snoc)),                       rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => ConsPASnoc(cons, pre                    , SNil :+ SnoC(snoc) :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, pre :>: rev(SnoC(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, pre :>: rev(SnoC(snoc)),                       rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, pre :>: rev(SnoC(snoc)),                       rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => ConsPA    (cons, pre                    , SNil :+ SnoC(snoc) :+ rev(           p)         )
      case A         (      a   ) => ConsPA    (cons, pre,                             SnoC(snoc)                      :<: a   )
      case PA        (   p, a   ) => ConsPA    (cons, pre :>: rev(SnoC(snoc)),                       rev(           p) :<: a   )
      case Snoc      (         s) => ConsPASnoc(cons, pre                    , SNil :+ SnoC(snoc)                           , s)
      case PSnoc     (   p,    s) => ConsPASnoc(cons, pre                    , SNil :+ SnoC(snoc) :+ rev(           p)      , s)
      case ASnoc     (      a, s) => ConsPASnoc(cons, pre                    ,         SnoC(snoc)                      :<: a, s)
      case PASnoc    (   p, a, s) => ConsPASnoc(cons, pre :>: rev(SnoC(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsPSnoc(cons, pre, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPSnoc(a +: cons, pre, snoc)


    override def map[B](f: (E) => B): InceptionQ[B] = ConsPSnoc(cons map f, pre mapCarry f, snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val c = cons.fold(zero)(f)
      val p = pre.foldCarry(c)(f)
      val s = snoc.fold(p)(f)
      s
    }

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) =>
          pre.nextConsBlock match {
            case (cns, Nil) =>
              h -> ConsSnoc(cns, snoc)
            case (cns, ph::pt) =>
              h -> ConsPSnoc(cns, ConsCarryList(Nel(ph, pt)), snoc)
          }
        case Nel(h, ch::ct) =>
          h -> ConsPSnoc(ConsList(Nel(ch, ct)), pre, snoc)
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) => ConsP(cons, pre) -> h
        case Nel(h, sh::st) => ConsPSnoc(cons, pre, SnocList(Nel(sh, st))) -> h
      }
    }

    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
      pre foreachCarry f
      snoc foreach f
    }
  }


  final case class ConsASnoc[E](cons: ConsList[E], app: SnocCarryList[E], snoc: SnocList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ConsASnoc(${cons.size},_,${app.toStructure},${snoc.size})"

    override def size: Int = cons.size + app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsASnoc (cons,                                 app,                                   snoc :+ s)
      case Cons      (c         ) => ConsPA    (cons, rev(app              ) +: CNil, SNil :+ SnoC(snoc) :+ rev(Cons(c)     )         )
      case ConsP     (c, p      ) => ConsPA    (cons, rev(app :+ SnoC(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => ConsPA    (cons, rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a   )
      case ConsPA    (c, p, a   ) => ConsPA    (cons, rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => ConsPASnoc(cons, rev(app              ) +: CNil, SNil :+ SnoC(snoc) :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, rev(app :+ SnoC(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => ConsPA    (cons, rev(app              ) +: CNil, SNil :+ SnoC(snoc) :+ rev(           p)         )
      case A         (      a   ) => ConsPA    (cons, rev(app              ) +: CNil,         SnoC(snoc)                      :<: a   )
      case PA        (   p, a   ) => ConsPA    (cons, rev(app :+ SnoC(snoc)) +: CNil,                       rev(           p) :<: a   )
      case Snoc      (         s) => ConsASnoc (cons,                                 app  :+ SnoC(snoc)                           , s)
      case PSnoc     (   p,    s) => ConsPASnoc(cons, rev(app :+ SnoC(snoc)) +: CNil, SNil :+               rev(           p)      , s)
      case ASnoc     (      a, s) => ConsPASnoc(cons, rev(app              ) +: CNil,         SnoC(snoc)                      :<: a, s)
      case PASnoc    (   p, a, s) => ConsPASnoc(cons, rev(app              ) +: CNil,                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsASnoc(cons, app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsASnoc(a +: cons, app, snoc)

    override def map[B](f: (E) => B): InceptionQ[B] = ConsASnoc(cons map f, app mapCarry f, snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val c = cons.fold(zero)(f)
      val a = app.foldCarry(c)(f)
      val s = snoc.fold(a)(f)
      s
    }

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) =>
          app.reverse.nextConsBlock match {
            case (cns, Nil) =>
              h -> ConsSnoc(cns, snoc)
            case (cns, ph::pt) =>
              h -> ConsPSnoc(cns, ConsCarryList(Nel(ph, pt)), snoc)
          }
        case Nel(h, ch::ct) =>
          h -> ConsASnoc(ConsList(Nel(ch, ct)), app, snoc)
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) => ConsA(cons, app) -> h
        case Nel(h, sh::st) => ConsASnoc(cons, app, SnocList(Nel(sh, st))) -> h
      }
    }

    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
      app foreachCarry f
      snoc foreach f
    }
  }


  final case class ConsPASnoc[E](cons: ConsList[E], pre: ConsCarryList[E], app: SnocCarryList[E], snoc: SnocList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ConsPASnoc(${cons.size},${pre.toStructure},${app.toStructure},${snoc.size})"

    override def size: Int = cons.size + pre.carrySize + app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsPASnoc(cons, pre,                            app,                                   snoc :+ s)
      case c@Cons    (_         ) => ConsPA    (cons, pre :>: rev(app              ), SNil :+ SnoC(snoc) :+ rev(     c      )         )
      case ConsP     (c, p      ) => ConsPA    (cons, pre :>: rev(app :+ SnoC(snoc)), SNil :+               rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => ConsPA    (cons, pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c))      :<: a   )
      case ConsPA    (c, p, a   ) => ConsPA    (cons, pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => ConsPASnoc(cons, pre :>: rev(app              ), SNil :+ SnoC(snoc) :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, pre :>: rev(app :+ SnoC(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c))      :<: a, s)
      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => ConsPA    (cons, pre                           , app  :+ SnoC(snoc) :+ rev(           p)         )
      case A         (      a   ) => ConsPA    (cons, pre :>: rev(app              ),         SnoC(snoc)                      :<: a   )
      case PA        (   p, a   ) => ConsPA    (cons, pre :>: rev(app :+ SnoC(snoc)),                       rev(           p) :<: a   )
      case Snoc      (         s) => ConsPASnoc(cons, pre                           , app  :+ SnoC(snoc)                           , s)
      case PSnoc     (   p,    s) => ConsPASnoc(cons, pre                           , app  :+ SnoC(snoc) :+ rev(           p)      , s)
      case ASnoc     (      a, s) => ConsPASnoc(cons, pre :>: rev(app :+ SnoC(snoc)),                                             a, s)
      case PASnoc    (   p, a, s) => ConsPASnoc(cons, pre :>: rev(app :+ SnoC(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsPASnoc(cons, pre, app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPASnoc(a +: cons, pre, app, snoc)

    override def map[B](f: (E) => B): InceptionQ[B] = ConsPASnoc(cons map f, pre mapCarry f, app mapCarry f, snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val c = cons.fold(zero)(f)
      val p = pre.foldCarry(c)(f)
      val a = app.foldCarry(p)(f)
      val s = snoc.fold(a)(f)
      s
    }

    override def uncons: Option[(E, InceptionQ[E])] = Some {
      cons._l match {
        case Nel(h, Nil) =>
          h -> {
            pre.nextConsBlock match {
              case (cns, Nil) =>
                ConsASnoc(cns, app, snoc)
              case (cns, ph::pt) =>
                ConsPASnoc(cns, ConsCarryList(Nel(ph, pt)), app, snoc)
            }
          }
        case Nel(h, ch::ct) =>
          h -> ConsPASnoc(ConsList(Nel(ch, ct)), pre, app, snoc)
      }
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) => ConsPA(cons, pre, app) -> h
        case Nel(h, sh::st) => ConsPASnoc(cons, pre, app, SnocList(Nel(sh, st))) -> h
      }
    }

    override def foreach[B](f: (E) => B): Unit = {
      cons foreach f
      pre foreachCarry f
      app foreachCarry f
      snoc foreach f
    }
  }

  final case class P[E](pre: ConsCarryList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"P(_,${pre.toStructure},_,_)"

    override def size: Int = pre.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PSnoc (pre,                          SNil :+ s)
      case c@Cons    (_         ) => PA    (pre, SNil :+ rev(c)                    )
      case ConsP     (c, p      ) => PA    (pre, SNil :+ rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => PA    (pre,         rev(Cons(c)     ) :<: a   )
      case ConsPA    (c, p, a   ) => PA    (pre,         rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => PASnoc(pre, SNil :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => PASnoc(pre, SNil :+ rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => PASnoc(pre,         rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => PASnoc(pre,         rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => PA    (pre, SNil :+ rev(           p)         )
      case A         (      a   ) => PA    (pre,                               a   )
      case PA        (   p, a   ) => PA    (pre,         rev(           p) :<: a   )
      case Snoc      (         s) => PSnoc (pre,                                  s)
      case PSnoc     (   p,    s) => PASnoc(pre, SNil :+ rev(           p)      , s)
      case ASnoc     (      a, s) => PASnoc(pre,                               a, s)
      case PASnoc    (   p, a, s) => PASnoc(pre,         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = PSnoc(pre, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsP(a +: CNil, pre)

    override def map[B](f: (E) => B): InceptionQ[B] = P(pre mapCarry f)

    override def fold[B](zero: B)(f: (B, E) => B): B = pre.foldCarry(zero)(f)

    override def uncons: Option[(E, InceptionQ[E])] = pre.nextConsBlock match {
      case (cns, Nil) =>
        Cons(cns).uncons
      case (cns, ph::pt) =>
        ConsP(cns, ConsCarryList(Nel(ph, pt))).uncons
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = pre.reverse.nextSnocBlock match {
      case (Nil, snc) =>
        Snoc(snc).unsnoc
      case (ah::at, snc) =>
        ASnoc(SnocCarryList(Nel(ah, at)), snc).unsnoc
    }

    override def foreach[B](f: (E) => B): Unit = {
      pre foreachCarry f
    }
  }

  final case class A[E](app: SnocCarryList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"A(_,_,${app.toStructure},_)"

    override def size: Int = app.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ASnoc (                  app,                     SNil :+ s)
      case c@Cons    (_         ) => A     (                  app  :+ rev(c           )         )
      case ConsP     (c, p      ) => A     (                  app  :+ rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => PA    (rev(app) +: CNil,                               a   )
      case ConsPA    (c, p, a   ) => PA    (rev(app) +: CNil,         rev(           p) :<: a   )
      case ConsSnoc  (c,       s) => ASnoc (                  app  :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => ASnoc (                  app  :+ rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => PASnoc(rev(app) +: CNil,         rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => PASnoc(rev(app) +: CNil,         rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => PA    (rev(app) +: CNil, SNil :+ rev(           p)         )
      case A         (      a   ) => PA    (rev(app) +: CNil,                               a   )
      case PA        (   p, a   ) => PA    (rev(app) +: CNil,         rev(           p) :<: a   )
      case Snoc      (         s) => ASnoc (                  app                            , s)
      case PSnoc     (   p,    s) => PASnoc(rev(app) +: CNil, SNil :+ rev(           p)      , s)
      case ASnoc     (      a, s) => PASnoc(rev(app) +: CNil,                               a, s)
      case PASnoc    (   p, a, s) => PASnoc(rev(app) +: CNil,         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ASnoc(app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsA(a +: CNil, app)

    override def map[B](f: (E) => B): InceptionQ[B] = A(app mapCarry f)

    override def fold[B](zero: B)(f: (B, E) => B): B = app.foldCarry(zero)(f)

    override def uncons: Option[(E, InceptionQ[E])] = app.reverse.nextConsBlock match {
      case (cns, Nil) =>
        Cons(cns).uncons
      case (cns, ph::pt) =>
        ConsP(cns, ConsCarryList(Nel(ph, pt))).uncons
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = app.nextSnocBlock match {
      case (Nil, snc) =>
        Snoc(snc).unsnoc
      case (ah::at, snc) =>
        ASnoc(SnocCarryList(Nel(ah, at)), snc).unsnoc
    }

    override def foreach[B](f: (E) => B): Unit = {
      app foreachCarry f
    }
  }

  final case class PA[E](pre: ConsCarryList[E], app: SnocCarryList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"PA(_,${pre.toStructure},${app.toStructure},_)"

    override def size: Int = pre.carrySize + app.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PASnoc(pre             , app,                     SNil :+ s)
      case c@Cons    (_         ) => PA    (pre             , app  :+ rev(c           )         )
      case ConsP     (c, p      ) => PA    (pre             , app  :+ rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => PA    (pre :>: rev(app),                               a   )
      case ConsPA    (c, p, a   ) => PA    (pre :>: rev(app),         rev(           p) :<: a   )
      case ConsSnoc  (c,       s) => PASnoc(pre             , app  :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => PASnoc(pre :>: rev(app), SNil :+ rev(           p)      , s)
      case ConsASnoc (c,    a, s) => PASnoc(pre :>: rev(app),         rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => PASnoc(pre :>: rev(app),         rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => PA    (pre             , app  :+ rev(           p)         )
      case A         (      a   ) => PA    (pre :>: rev(app),                               a   )
      case PA        (   p, a   ) => PA    (pre :>: rev(app),         rev(           p) :<: a   )
      case Snoc      (         s) => PASnoc(pre             , app                            , s)
      case PSnoc     (   p,    s) => PASnoc(pre             , app  :+ rev(           p)      , s)
      case ASnoc     (      a, s) => PASnoc(pre :>: rev(app),                               a, s)
      case PASnoc    (   p, a, s) => PASnoc(pre :>: rev(app),         rev(           p) :<: a, s)
    }


    override def :+(a: E): InceptionQ[E] = PASnoc(pre, app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPA(a +: CNil, pre, app)

    override def map[B](f: (E) => B): InceptionQ[B] = PA(pre mapCarry f, app mapCarry f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val p = pre.foldCarry(zero)(f)
      val a = app.foldCarry(p)(f)
      a
    }

    override def uncons: Option[(E, InceptionQ[E])] = pre.nextConsBlock match {
      case (cns, Nil) =>
        ConsA(cns, app).uncons
      case (cns, ph::pt) =>
        ConsPA(cns, ConsCarryList(Nel(ph, pt)), app).uncons
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = app.nextSnocBlock match {
      case (Nil, snc) =>
        PSnoc(pre, snc).unsnoc
      case (ah::at, snc) =>
        PASnoc(pre, SnocCarryList(Nel(ah, at)), snc).unsnoc
    }

    override def foreach[B](f: (E) => B): Unit = {
      pre foreachCarry f
      app foreachCarry f
    }
  }

  def SnoC[E](snoc: SnocList[E]): SnocCarry[E] = Snoc(snoc)

  final case class Snoc[E](snoc: SnocList[E]) extends InceptionQ[E] with SnocCarry[E] {

    override def toStructure: String = s"Snoc(_,_,_,${snoc.size})"

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ASnoc (                         SNil :+ SnoC(snoc),                     SNil :+ s)
      case c@Cons    (_         ) => A     (                         SNil :+ SnoC(snoc) :+  rev(     c      )         )
      case ConsP     (c, p      ) => A     (                         SNil :+ SnoC(snoc) :+  rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => PA    (rev(SnoC(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a   )
      case ConsPA    (c, p, a   ) => PA    (rev(SnoC(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => PASnoc(rev(SnoC(snoc)) +: CNil, SNil               :+  rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => PASnoc(rev(SnoC(snoc)) +: CNil, SNil               :+  rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => PASnoc(rev(SnoC(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => PASnoc(rev(SnoC(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => A     (                         SNil :+ SnoC(snoc) :+  rev(           p)         )
      case A         (      a   ) => A     (                                 SnoC(snoc)                       :<: a   )
      case PA        (   p, a   ) => PA    (rev(SnoC(snoc)) +: CNil,                        rev(           p) :<: a   )
      case Snoc      (         s) => ASnoc (                         SNil :+ SnoC(snoc)                            , s)
      case PSnoc     (   p,    s) => PASnoc(rev(SnoC(snoc)) +: CNil, SNil :+                rev(           p)      , s)
      case ASnoc     (      a, s) => ASnoc (                                 SnoC(snoc)                       :<: a, s)
      case PASnoc    (   p, a, s) => PASnoc(rev(SnoC(snoc)) +: CNil,                        rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = Snoc(snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsSnoc(a +: CNil, snoc)

    override def size: Int = snoc.size

    override def map[B](f: (E) => B): Snoc[B] = Snoc(snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = snoc.fold(zero)(f)

    override def reverse: ConsCarry[E] = Cons(snoc.reverse)

    override def uncons: Option[(E, InceptionQ[E])] = Cons(snoc.reverse).uncons

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) => empty[E] -> h
        case Nel(h, sh::st) => Snoc(SnocList(Nel(sh, st))) -> h
      }
    }


    override def nextSnocBlock: (List[SnocCarry[E]], SnocList[E]) = (Nil, snoc)

    override def foreach[B](f: (E) => B): Unit = {
      snoc foreach f
    }
  }

  final case class PSnoc[E](pre: ConsCarryList[E], snoc: SnocList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"PSnoc(_,${pre.toStructure},_,${snoc.size})"

    override def size: Int = pre.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PASnoc(pre                    , SNil :+ SnoC(snoc),                    SNil :+ s)
      case Cons      (c         ) => PA    (pre                    , SNil :+ SnoC(snoc) :+ rev(Cons(c)     )         )
      case ConsP     (c, p      ) => PA    (pre                    , SNil :+ SnoC(snoc) :+ rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => PA    (pre :>: rev(SnoC(snoc)),                       rev(Cons(c)     ) :<: a   )
      case ConsPA    (c, p, a   ) => PA    (pre :>: rev(SnoC(snoc)),                       rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => PASnoc(pre                    , SNil :+ SnoC(snoc) :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => PASnoc(pre :>: rev(SnoC(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => PASnoc(pre :>: rev(SnoC(snoc)),                       rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => PASnoc(pre :>: rev(SnoC(snoc)),                       rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => PA    (pre                    , SNil :+ SnoC(snoc) :+ rev(           p)         )
      case A         (      a   ) => PA    (pre,                             SnoC(snoc) :<:                      a   )
      case PA        (   p, a   ) => PA    (pre :>: rev(SnoC(snoc)),                       rev(           p) :<: a   )
      case Snoc      (         s) => PASnoc(pre :>: rev(SnoC(snoc)), SNil :+ SnoC(snoc)                           , s)
      case PSnoc     (   p,    s) => PASnoc(pre :>: rev(SnoC(snoc)), SNil :+               rev(           p)      , s)
      case ASnoc     (      a, s) => PASnoc(pre :>: rev(SnoC(snoc)),                                             a, s)
      case PASnoc    (   p, a, s) => PASnoc(pre :>: rev(SnoC(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = PSnoc(pre, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPSnoc(a +: CNil, pre, snoc)

    override def map[B](f: (E) => B): InceptionQ[B] = PSnoc(pre mapCarry f, snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val p = pre.foldCarry(zero)(f)
      val s = snoc.fold(p)(f)
      s
    }

    override def uncons: Option[(E, InceptionQ[E])] = pre.nextConsBlock match {
      case (cns, Nil) =>
        ConsSnoc(cns, snoc).uncons
      case (cns, ph::pt) =>
        ConsPSnoc(cns, ConsCarryList(Nel(ph, pt)), snoc).uncons
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) => P(pre) -> h
        case Nel(h, sh::st) => PSnoc(pre, SnocList(Nel(sh, st))) -> h
      }
    }

    override def foreach[B](f: (E) => B): Unit = {
      pre foreachCarry f
      snoc foreach f
    }
  }

  final case class ASnoc[E](app: SnocCarryList[E], snoc: SnocList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"ASnoc(_,_,${app.toStructure},${snoc.size})"

    override def size: Int = app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ASnoc (                                app,                                   snoc :+ s)
      case Cons      (c         ) => PA    (rev(app              ) +: CNil, SNil :+ SnoC(snoc) :+ rev(Cons(c)     )         )
      case ConsP     (c, p      ) => PA    (rev(app :+ SnoC(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => PA    (rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a   )
      case ConsPA    (c, p, a   ) => PA    (rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => PASnoc(rev(app              ) +: CNil, SNil :+ SnoC(snoc) :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => PASnoc(rev(app :+ SnoC(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => PASnoc(rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a, s)
      case ConsPASnoc(c, p, a, s) => PASnoc(rev(app :+ SnoC(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => PA    (rev(app              ) +: CNil, SNil :+ SnoC(snoc) :+ rev(           p)         )
      case A         (      a   ) => PA    (rev(app              ) +: CNil,         SnoC(snoc)                      :<: a   )
      case PA        (   p, a   ) => PA    (rev(app :+ SnoC(snoc)) +: CNil,                       rev(           p) :<: a   )
      case Snoc      (         s) => ASnoc (                                app  :+ SnoC(snoc)                           , s)
      case PSnoc     (   p,    s) => PASnoc(rev(app :+ SnoC(snoc)) +: CNil, SNil :+               rev(           p)      , s)
      case ASnoc     (      a, s) => PASnoc(rev(app              ) +: CNil,         SnoC(snoc)                      :<: a, s)
      case PASnoc    (   p, a, s) => PASnoc(rev(app              ) +: CNil,                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ASnoc(app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsASnoc(a +: CNil, app, snoc)

    override def map[B](f: (E) => B): InceptionQ[B] = ASnoc(app mapCarry f, snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val a = app.foldCarry(zero)(f)
      val s = snoc.fold(a)(f)
      s
    }

    override def uncons: Option[(E, InceptionQ[E])] = app.reverse.nextConsBlock match {
      case (cns, Nil) =>
        ConsSnoc(cns, snoc).uncons
      case (cns, ph::pt) =>
        ConsPSnoc(cns, ConsCarryList(Nel(ph, pt)), snoc).uncons
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) => A(app) -> h
        case Nel(h, sh::st) => ASnoc(app, SnocList(Nel(sh, st))) -> h
      }
    }

    override def foreach[B](f: (E) => B): Unit = {
      app foreachCarry f
      snoc foreach f
    }

  }

  final case class PASnoc[E](pre: ConsCarryList[E], app: SnocCarryList[E], snoc: SnocList[E]) extends InceptionQ[E] {

    override def toStructure: String = s"PASnoc(_,${pre.toStructure},${app.toStructure},${snoc.size})"

    override def size: Int = pre.carrySize + app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PASnoc(pre,                            app,                                   snoc :+ s)
      case c@Cons    (_         ) => PA    (pre :>: rev(app              ), SNil :+ SnoC(snoc) :+ rev(     c      )         )
      case ConsP     (c, p      ) => PA    (pre :>: rev(app :+ SnoC(snoc)), SNil :+               rev(Cons(c) +: p)         )
      case ConsA     (c,    a   ) => PA    (pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c))      :<: a   )
      case ConsPA    (c, p, a   ) => PA    (pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c) +: p) :<: a   )
      case ConsSnoc  (c,       s) => PASnoc(pre :>: rev(app              ), SNil :+ SnoC(snoc) :+ rev(Cons(c)     )      , s)
      case ConsPSnoc (c, p,    s) => PASnoc(pre :>: rev(app :+ SnoC(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
      case ConsASnoc (c,    a, s) => PASnoc(pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c))      :<: a, s)
      case ConsPASnoc(c, p, a, s) => PASnoc(pre :>: rev(app :+ SnoC(snoc)),                       rev(Cons(c) +: p) :<: a, s)
      case QNil()                 => this
      case P         (   p      ) => PA    (pre                           , app  :+ SnoC(snoc) :+ rev(           p)         )
      case A         (      a   ) => PA    (pre :>: rev(app              ),         SnoC(snoc)                      :<: a   )
      case PA        (   p, a   ) => PA    (pre :>: rev(app :+ SnoC(snoc)),                       rev(           p) :<: a   )
      case Snoc      (         s) => PASnoc(pre                           , app  :+ SnoC(snoc)                           , s)
      case PSnoc     (   p,    s) => PASnoc(pre                           , app  :+ SnoC(snoc) :+ rev(           p)      , s)
      case ASnoc     (      a, s) => PASnoc(pre :>: rev(app :+ SnoC(snoc)),                                             a, s)
      case PASnoc    (   p, a, s) => PASnoc(pre :>: rev(app :+ SnoC(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = PASnoc(pre, app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPASnoc(a +: CNil, pre, app, snoc)

    override def map[B](f: (E) => B): InceptionQ[B] = PASnoc(pre mapCarry f, app mapCarry f, snoc map f)

    override def fold[B](zero: B)(f: (B, E) => B): B = {
      val p = pre.foldCarry(zero)(f)
      val a = app.foldCarry(p)(f)
      val s = snoc.fold(a)(f)
      s
    }

    override def uncons: Option[(E, InceptionQ[E])] = pre.nextConsBlock match {
      case (cns, Nil) =>
        ConsASnoc(cns, app, snoc).uncons
      case (cns, ph::pt) =>
        ConsPASnoc(cns, ConsCarryList(ph, pt), app, snoc).uncons
    }

    override def unsnoc: Option[(InceptionQ[E], E)] = Some {
      snoc._l match {
        case Nel(h, Nil) => PA(pre, app) -> h
        case Nel(h, sh::st) => PASnoc(pre, app, SnocList(Nel(sh, st))) -> h
      }
    }


    override def foreach[B](f: (E) => B): Unit = {
      pre foreachCarry f
      app foreachCarry f
      snoc foreach f
    }
  }
}