package scabs.seq

import shapeless.newtype
import shapeless.newtype._

trait InceptionQ[E] {
  def size: Int

  def isEmpty: Boolean

  def ++(rhs: InceptionQ[E]): InceptionQ[E]

  def :+(a: E): InceptionQ[E]

  def +:(a: E): InceptionQ[E]
//
//  def headTail: Option[(A, InceptionQ[E])]
//
//  def firstLast: Option[(InceptionQ[E], A)]
//
//  def map[B](f: A => B): InceptionQ[B]
//
//  def fold[B](zero: B)(f: (B, A) => B): B
}

object InceptionQ {
  // singleton hack
  
  def empty[E]: InceptionQ[E] = QNil.asInstanceOf[InceptionQ[E]]

  private type Nel[+E] = List[E]
  private object Nel {
    @inline def apply[E](e: E): Nel[E] = e::Nil
    @inline def apply[E](e: E, es: List[E]): Nel[E] = e::es
  }

  private type ConsList[E] = Newtype[Nel[E], ConsListOps[E]]
  private class ConsListOps[E](val _l: Nel[E]) extends AnyVal {
    @inline def +:(e: E): ConsList[E] = newtype(Nel(e, _l))
    @inline def size: Int = _l.size
    @inline def carrySize[C](implicit ev: E <:< ConsCarry[C]): Int = _l.foldLeft(0)((a, f) => a + f.size)
  }
  private implicit def consListOps[E](l: Nel[E]): ConsListOps[E] = new ConsListOps(l)
  private object CNil {
    @inline def +: [E](e: E): ConsList[E] = newtype(Nel(e))
  }

  private type SnocList[E] = Newtype[Nel[E], SnocListOps[E]]
  private class SnocListOps[E](val _l: Nel[E]) extends AnyVal {
    @inline def :+(e: E): SnocList[E] = newtype(Nel(e, _l))
    @inline def size: Int = _l.size
    @inline def :<: [C](e: E)(implicit ev: E <:< SnocCarry[C]): SnocList[C] = ???
    @inline def carrySize[C](implicit ev: E <:< SnocCarry[C]): Int = _l.foldLeft(0)((a, f) => a + f.size)
  }
  private implicit def snocListOps[E](l: Nel[E]): SnocListOps[E] = new SnocListOps(l)
  private object SNil {
    @inline def :+ [E](e: E): SnocList[E] = newtype(Nel(e))
  }


  private sealed trait Carry[E] {
    def size: Int
  }
  private sealed trait ConsCarry[E] extends Carry[E] {
    final def :>: (l: ConsList[ConsCarry[E]]): ConsList[ConsCarry[E]] = ???
  }
  private sealed trait SnocCarry[E] extends Carry[E]

  private case class ReverseS[E](c: SnocCarry[E]) extends ConsCarry[E] {

    override def size: Int = c.size
  }

  private case class ReverseSnoc[E](cs: SnocList[SnocCarry[E]]) extends ConsCarry[E] {
    override def size: Int = cs.carrySize
  }

  private case class ReverseC[E](c: ConsCarry[E]) extends SnocCarry[E] {
    override def size: Int = c.size
  }

  private case class ReverseCons[E](cs: ConsList[ConsCarry[E]]) extends SnocCarry[E] {
    override def size: Int = cs.carrySize
  }

  private def rev[E](s: SnocCarry[E]): ConsCarry[E] = ReverseS(s)
  private def rev[E](c: ConsCarry[E]): SnocCarry[E] = ReverseC(c)
  private def rev[E](l: ConsList[ConsCarry[E]]): SnocCarry[E] = ReverseCons(l)
  private def rev[E](l: SnocList[SnocCarry[E]]): ConsCarry[E] = ReverseSnoc(l)


  case class QNil[E]() extends InceptionQ[E] {
    override def size: Int = 0

    override def isEmpty: Boolean = true

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs

    override def :+(a: E): InceptionQ[E] = Snoc(SNil :+ a)

    override def +:(a: E): InceptionQ[E] = Cons(a +: CNil)

//    override def headTail: Option[(Nothing, InceptionQ[Nothing])] = None
//
//    override def firstLast: Option[(InceptionQ[Nothing], Nothing)] = None
//
//    override def map[B](f: (Nothing) => B): InceptionQ[B] = empty[B]
//
//    override def fold[B](zero: B)(f: (B, Nothing) => B): B = zero
  }

  private case class Unity[E](u: E) extends InceptionQ[E] with SnocCarry[E] with ConsCarry[E] {

    override def size: Int = 1

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsSnoc  (u +: CNil, SNil :+ s)
      case Cons      (c         ) => Cons      (u +: c          )
      case ConsP     (c, p      ) => ConsP     (u +:    c, p        )
      case ConsA     (c,    a   ) => ConsA     (u +:    c,    a     )
      case ConsPA    (c, p, a   ) => ConsPA    (u +:    c, p, a     )
      case ConsSnoc  (c,       s) => ConsSnoc  (u +:    c,         s)
      case ConsPSnoc (c, p   , s) => ConsPSnoc (u +:    c, p,      s)
      case ConsASnoc (c,    a, s) => ConsASnoc (u +:    c,    a,   s)
      case ConsPASnoc(c, p, a, s) => ConsPASnoc(u +:    c, p, a,   s)
      case QNil()                   => this
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

  }

  private case class Cons[E](cons: ConsList[E]) extends InceptionQ[E] with ConsCarry[E] {
    override def size: Int = cons.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
        case Unity     (         s) => ConsSnoc  (cons,             SNil :+ s)
        case c@Cons    (_         ) => ConsA     (cons,        SNil :+ rev(c))
        case ConsP     (c, p      ) => ConsP     (cons, Cons(c) +:    p      )
        case ConsA     (c,    a   ) => ConsPA    (cons, Cons(c) +: CNil, a   )
        case ConsPA    (c, p, a   ) => ConsPA    (cons, Cons(c) +:    p, a   )
        case ConsSnoc  (c,       s) => ConsPSnoc (cons, Cons(c) +: CNil,    s)
        case ConsPSnoc (c, p,    s) => ConsPSnoc (cons, Cons(c) +:    p,    s)
        case ConsASnoc (c,    a, s) => ConsPASnoc(cons, Cons(c) +: CNil, a, s)
        case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, Cons(c) +:    p, a, s)
        case QNil()                   => this
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
  }

  private case class ConsP[E](cons: ConsList[E], pre: ConsList[ConsCarry[E]]) extends InceptionQ[E] {
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
        case QNil()                   => this
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
  }

  private case class ConsA[E](cons: ConsList[E], app: SnocList[SnocCarry[E]]) extends InceptionQ[E] {
    override def size: Int = cons.size + app.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
        case Unity     (         s) => ConsASnoc (cons,                   app,                     SNil :+ s)
//        case c@Cons    (_         ) => ConsA     (cons,                   app  :+ rev(     c      )         )
//        case ConsP     (c, p      ) => ConsPA    (cons, rev(app) +: CNil, SNil :+ rev(Cons(c) +: p)         )
//        case ConsA     (c,    a   ) => ConsPA    (cons, rev(app) +: CNil,         rev(Cons(c)     ) :<: a   )
//        case ConsPA    (c, p, a   ) => ConsPA    (cons, rev(app) +: CNil,         rev(Cons(c) +: p) :<: a   )
//        case ConsSnoc  (c,       s) => ConsASnoc (cons,                   app  :+ rev(Cons(c)     )      , s)
//        case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, rev(app) +: CNil, SNil :+ rev(Cons(c) +: p)      , s)
//        case ConsASnoc (c,    a, s) => ConsPASnoc(cons, rev(app) +: CNil,         rev(Cons(c)     ) :<: a, s)
//        case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, rev(app) +: CNil,         rev(Cons(c) +: p) :<: a, s)
//        case QNil()                   => this
//        case P         (   p      ) => ConsA     (cons,                   app  :+ rev(           p)         )
//        case A         (      a   ) => ConsPA    (cons, rev(app) +: CNil,                               a   )
//        case PA        (   p, a   ) => ConsPA    (cons, rev(app) +: CNil,         rev(           p) :<: a   )
//        case Snoc      (         s) => ConsASnoc (cons,                   app,                             s)
//        case PSnoc     (   p,    s) => ConsASnoc (cons,                   app  :+ rev(           p)      , s)
//        case ASnoc     (      a, s) => ConsPASnoc(cons, rev(app) +: CNil,                               a, s)
//        case PASnoc    (   p, a, s) => ConsPASnoc(cons, rev(app) +: CNil,         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsASnoc(cons, app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsA(a +: cons, app)
  }

  private case class ConsPA[E](cons: ConsList[E], pre: ConsList[ConsCarry[E]], app: SnocList[SnocCarry[E]]) extends InceptionQ[E] {
    override def size: Int = cons.size + pre.carrySize + app.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsPASnoc(cons, pre                   , app                    , SNil :+ s)
//      case c@Cons    (_         ) => ConsPA    (cons, pre :>: rev(app), SNil :+ rev(     c      )         )
//      case ConsP     (c, p      ) => ConsPA    (cons, pre :>: rev(app), SNil :+ rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => ConsPA    (cons, pre             ,         rev(Cons(c)     ) :<: a   )
//      case ConsPA    (c, p, a   ) => ConsPA    (cons, pre :>: rev(app),         rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => ConsPASnoc(cons, pre :>: rev(app), SNil :+ rev(Cons(c) )          , s)
//      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, pre :>: rev(app), SNil :+ rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, pre :>: rev(app),         rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, pre :>: rev(app),         rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => ConsPA    (cons, pre :>: rev(app), SNil :+ rev(           p)         )
//      case A         (      a   ) => ConsPA    (cons, pre :>: rev(app),                               a   )
//      case PA        (   p, a   ) => ConsPA    (cons, pre :>: rev(app),         rev(           p) :<: a   )
//      case Snoc      (         s) => ConsPASnoc(cons, pre             ,                             app, s)
//      case PSnoc     (   p,    s) => ConsPASnoc(cons, pre             , app  :+ rev(           p)      , s)
//      case ASnoc     (      a, s) => ConsPASnoc(cons, pre :>: rev(app),                               a, s)
//      case PASnoc    (   p, a, s) => ConsPASnoc(cons, pre :>: rev(app),         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsPASnoc(cons, pre, app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPA(a +: cons, pre, app)
  }

  private case class ConsSnoc[E](cons: ConsList[E], snoc: SnocList[E]) extends InceptionQ[E] {
    override def size: Int = cons.size + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
        case Unity     (         s) => ConsASnoc (cons,                          SNil :+ Snoc(snoc),                     SNil :+ s)
//        case c@Cons    (_         ) => ConsA     (cons,                          SNil :+ Snoc(snoc) :+  rev(     c      )         )
//        case ConsP     (c, p      ) => ConsA     (cons,                          SNil :+ Snoc(snoc) :+  rev(Cons(c) +: p)         )
//        case ConsA     (c,    a   ) => ConsPA    (cons, rev(Snoc(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a   )
//        case ConsPA    (c, p, a   ) => ConsPA    (cons, rev(Snoc(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a   )
//        case ConsSnoc  (c,       s) => ConsASnoc (cons,                          SNil :+ Snoc(snoc) :+  rev(Cons(c)     )      , s)
//        case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, rev(Snoc(snoc)) +: CNil, SNil               :+  rev(Cons(c) +: p)      , s)
//        case ConsASnoc (c,    a, s) => ConsPASnoc(cons, rev(Snoc(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a, s)
//        case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, rev(Snoc(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a, s)
//        case QNil()                   => this
//        case P         (   p      ) => ConsA     (cons,                          SNil :+ Snoc(snoc) :+  rev(           p)         )
//        case A         (      a   ) => ConsA     (cons,                                  Snoc(snoc)                       :<: a   )
//        case PA        (   p, a   ) => ConsPA    (cons, rev(Snoc(snoc)) +: CNil,                        rev(           p) :<: a   )
//        case Snoc      (         s) => ConsASnoc (cons,                          SNil :+ Snoc(snoc)                            , s)
//        case PSnoc     (   p,    s) => ConsASnoc (cons,                          SNil :+ Snoc(snoc) :+  rev(           p)      , s)
//        case ASnoc     (      a, s) => ConsASnoc (cons,                                  Snoc(snoc)                       :<: a, s)
//        case PASnoc    (   p, a, s) => ConsPASnoc(cons, rev(Snoc(snoc)) +: CNil,                        rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsSnoc(cons, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsSnoc(a +: cons, snoc)
  }
//
  private case class ConsPSnoc[E](cons: ConsList[E], pre: ConsList[ConsCarry[E]], snoc: SnocList[E]) extends InceptionQ[E] {
    override def size: Int = cons.size + pre.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsPASnoc(cons, pre                    , SNil :+ Snoc(snoc),                    SNil :+ s)
//      case Cons      (c         ) => ConsPA    (cons, pre                    , SNil :+ Snoc(snoc) :+ rev(Cons(c)     )         )
//      case ConsP     (c, p      ) => ConsPA    (cons, pre                    , SNil :+ Snoc(snoc) :+ rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => ConsPA    (cons, pre :>: rev(Snoc(snoc)),                       rev(Cons(c)     ) :<: a   )
//      case ConsPA    (c, p, a   ) => ConsPA    (cons, pre :>: rev(Snoc(snoc)),                       rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => ConsPASnoc(cons, pre                    , SNil :+ Snoc(snoc) :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, pre :>: rev(Snoc(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, pre :>: rev(Snoc(snoc)),                       rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, pre :>: rev(Snoc(snoc)),                       rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => ConsPA    (cons, pre                    , SNil :+ Snoc(snoc) :+ rev(           p)         )
//      case A         (      a   ) => ConsPA    (cons, pre,                             Snoc(snoc)                      :<: a   )
//      case PA        (   p, a   ) => ConsPA    (cons, pre :>: rev(Snoc(snoc)),                       rev(           p) :<: a   )
//      case Snoc      (         s) => ConsPASnoc(cons, pre                    , SNil :+ Snoc(snoc)                           , s)
//      case PSnoc     (   p,    s) => ConsPASnoc(cons, pre                    , SNil :+ Snoc(snoc) :+ rev(           p)      , s)
//      case ASnoc     (      a, s) => ConsPASnoc(cons, pre                    ,         Snoc(snoc)                      :<: a, s)
//      case PASnoc    (   p, a, s) => ConsPASnoc(cons, pre :>: rev(Snoc(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsPSnoc(cons, pre, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPSnoc(a +: cons, pre, snoc)
  }


  private case class ConsASnoc[E](cons: ConsList[E], app: SnocList[SnocCarry[E]], snoc: SnocList[E]) extends InceptionQ[E] {
    override def size: Int = cons.size + app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsASnoc (cons,                                 app,                                   snoc :+ s)
//      case Cons      (c         ) => ConsPA    (cons, rev(app              ) +: CNil, SNil :+ Snoc(snoc) :+ rev(Cons(c)     )         )
//      case ConsP     (c, p      ) => ConsPA    (cons, rev(app :+ Snoc(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => ConsPA    (cons, rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a   )
//      case ConsPA    (c, p, a   ) => ConsPA    (cons, rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => ConsPASnoc(cons, rev(app              ) +: CNil, SNil :+ Snoc(snoc) :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, rev(app :+ Snoc(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => ConsPA    (cons, rev(app              ) +: CNil, SNil :+ Snoc(snoc) :+ rev(           p)         )
//      case A         (      a   ) => ConsPA    (cons, rev(app              ) +: CNil,         Snoc(snoc)                      :<: a   )
//      case PA        (   p, a   ) => ConsPA    (cons, rev(app :+ Snoc(snoc)) +: CNil,                       rev(           p) :<: a   )
//      case Snoc      (         s) => ConsASnoc (cons,                                 app  :+ Snoc(snoc)                           , s)
//      case PSnoc     (   p,    s) => ConsPASnoc(cons, rev(app :+ Snoc(snoc)) +: CNil, SNil :+               rev(           p)      , s)
//      case ASnoc     (      a, s) => ConsPASnoc(cons, rev(app              ) +: CNil,         Snoc(snoc)                      :<: a, s)
//      case PASnoc    (   p, a, s) => ConsPASnoc(cons, rev(app              ) +: CNil,                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsASnoc(cons, app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsASnoc(a +: cons, app, snoc)
  }


  private case class ConsPASnoc[E](cons: ConsList[E], pre: ConsList[ConsCarry[E]], app: SnocList[SnocCarry[E]], snoc: SnocList[E]) extends InceptionQ[E] {

    override def size: Int = cons.size + pre.carrySize + app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ConsPASnoc(cons, pre,                            app,                                   snoc :+ s)
//      case c@Cons    (_         ) => ConsPA    (cons, pre :>: rev(app              ), SNil :+ Snoc(snoc) :+ rev(     c      )         )
//      case ConsP     (c, p      ) => ConsPA    (cons, pre :>: rev(app :+ Snoc(snoc)), SNil :+               rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => ConsPA    (cons, pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c))      :<: a   )
//      case ConsPA    (c, p, a   ) => ConsPA    (cons, pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => ConsPASnoc(cons, pre :>: rev(app              ), SNil :+ Snoc(snoc) :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => ConsPASnoc(cons, pre :>: rev(app :+ Snoc(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => ConsPASnoc(cons, pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c))      :<: a, s)
//      case ConsPASnoc(c, p, a, s) => ConsPASnoc(cons, pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => ConsPA    (cons, pre                           , app  :+ Snoc(snoc) :+ rev(           p)         )
//      case A         (      a   ) => ConsPA    (cons, pre :>: rev(app              ),         Snoc(snoc)                      :<: a   )
//      case PA        (   p, a   ) => ConsPA    (cons, pre :>: rev(app :+ Snoc(snoc)),                       rev(           p) :<: a   )
//      case Snoc      (         s) => ConsPASnoc(cons, pre                           , app  :+ Snoc(snoc)                           , s)
//      case PSnoc     (   p,    s) => ConsPASnoc(cons, pre                           , app  :+ Snoc(snoc) :+ rev(           p)      , s)
//      case ASnoc     (      a, s) => ConsPASnoc(cons, pre :>: rev(app :+ Snoc(snoc)),                                             a, s)
//      case PASnoc    (   p, a, s) => ConsPASnoc(cons, pre :>: rev(app :+ Snoc(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ConsPASnoc(cons, pre, app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPASnoc(a +: cons, pre, app, snoc)
  }

  private case class P[E](pre: ConsList[ConsCarry[E]]) extends InceptionQ[E] {
    override def size: Int = pre.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PSnoc (pre,                          SNil :+ s)
//      case c@Cons    (_         ) => PA    (pre, SNil :+ rev(c)                    )
//      case ConsP     (c, p      ) => PA    (pre, SNil :+ rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => PA    (pre,         rev(Cons(c)     ) :<: a   )
//      case ConsPA    (c, p, a   ) => PA    (pre,         rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => PASnoc(pre, SNil :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => PASnoc(pre, SNil :+ rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => PASnoc(pre,         rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => PASnoc(pre,         rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => PA    (pre, SNil :+ rev(           p)         )
//      case A         (      a   ) => PA    (pre,                               a   )
//      case PA        (   p, a   ) => PA    (pre,         rev(           p) :<: a   )
//      case Snoc      (         s) => PSnoc (pre,                                  s)
//      case PSnoc     (   p,    s) => PASnoc(pre, SNil :+ rev(           p)      , s)
//      case ASnoc     (      a, s) => PASnoc(pre,                               a, s)
//      case PASnoc    (   p, a, s) => PASnoc(pre,         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = PSnoc(pre, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsP(a +: CNil, pre)
  }
//
  private case class A[E](app: SnocList[SnocCarry[E]]) extends InceptionQ[E] {
    override def size: Int = app.carrySize

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ASnoc (                  app,                     SNil :+ s)
//      case c@Cons    (_         ) => A     (                  app  :+ rev(c           )         )
//      case ConsP     (c, p      ) => A     (                  app  :+ rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => PA    (rev(app) +: CNil,                               a   )
//      case ConsPA    (c, p, a   ) => PA    (rev(app) +: CNil,         rev(           p) :<: a   )
//      case ConsSnoc  (c,       s) => ASnoc (                  app  :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => ASnoc (                  app  :+ rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => PASnoc(rev(app) +: CNil,         rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => PASnoc(rev(app) +: CNil,         rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => PA    (rev(app) +: CNil, SNil :+ rev(           p)         )
//      case A         (      a   ) => PA    (rev(app) +: CNil,                               a   )
//      case PA        (   p, a   ) => PA    (rev(app) +: CNil,         rev(           p) :<: a   )
//      case Snoc      (         s) => ASnoc (                  app                            , s)
//      case PSnoc     (   p,    s) => PASnoc(rev(app) +: CNil, SNil :+ rev(           p)      , s)
//      case ASnoc     (      a, s) => PASnoc(rev(app) +: CNil,                               a, s)
//      case PASnoc    (   p, a, s) => PASnoc(rev(app) +: CNil,         rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ASnoc(app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsA(a +: CNil, app)
  }

  private case class PA[E](pre: ConsList[ConsCarry[E]], app: SnocList[SnocCarry[E]]) extends InceptionQ[E] {
    override def size: Int = pre.carrySize + app.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PASnoc(pre             , app,                     SNil :+ s)
//      case c@Cons    (_         ) => PA    (pre             , app  :+ rev(c           )         )
//      case ConsP     (c, p      ) => PA    (pre             , app  :+ rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => PA    (pre :>: rev(app),                               a   )
//      case ConsPA    (c, p, a   ) => PA    (pre :>: rev(app),         rev(           p) :<: a   )
//      case ConsSnoc  (c,       s) => PASnoc(pre             , app  :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => PASnoc(pre :>: rev(app), SNil :+ rev(           p)      , s)
//      case ConsASnoc (c,    a, s) => PASnoc(pre :>: rev(app),         rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => PASnoc(pre :>: rev(app),         rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => PA    (pre             , app  :+ rev(           p)         )
//      case A         (      a   ) => PA    (pre :>: rev(app),                               a   )
//      case PA        (   p, a   ) => PA    (pre :>: rev(app),         rev(           p) :<: a   )
//      case Snoc      (         s) => PASnoc(pre             , app                            , s)
//      case PSnoc     (   p,    s) => PASnoc(pre             , app  :+ rev(           p)      , s)
//      case ASnoc     (      a, s) => PASnoc(pre :>: rev(app),                               a, s)
//      case PASnoc    (   p, a, s) => PASnoc(pre :>: rev(app),         rev(           p) :<: a, s)
    }


    override def :+(a: E): InceptionQ[E] = PASnoc(pre, app, SNil :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPA(a +: CNil, pre, app)
  }

  private case class Snoc[E](snoc: SnocList[E]) extends InceptionQ[E] with SnocCarry[E] {
    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ASnoc (                         SNil :+ Snoc(snoc),                     SNil :+ s)
//      case c@Cons    (_         ) => A     (                         SNil :+ Snoc(snoc) :+  rev(     c      )         )
//      case ConsP     (c, p      ) => A     (                         SNil :+ Snoc(snoc) :+  rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => PA    (rev(Snoc(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a   )
//      case ConsPA    (c, p, a   ) => PA    (rev(Snoc(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => PASnoc(rev(Snoc(snoc)) +: CNil, SNil               :+  rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => PASnoc(rev(Snoc(snoc)) +: CNil, SNil               :+  rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => PASnoc(rev(Snoc(snoc)) +: CNil,                        rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => PASnoc(rev(Snoc(snoc)) +: CNil,                        rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => A     (                         SNil :+ Snoc(snoc) :+  rev(           p)         )
//      case A         (      a   ) => A     (                                 Snoc(snoc)                       :<: a   )
//      case PA        (   p, a   ) => PA    (rev(Snoc(snoc)) +: CNil,                        rev(           p) :<: a   )
//      case Snoc      (         s) => ASnoc (                         SNil :+ Snoc(snoc)                            , s)
//      case PSnoc     (   p,    s) => PASnoc(rev(Snoc(snoc)) +: CNil, SNil :+                rev(           p)      , s)
//      case ASnoc     (      a, s) => ASnoc (                                 Snoc(snoc)                       :<: a, s)
//      case PASnoc    (   p, a, s) => PASnoc(rev(Snoc(snoc)) +: CNil,                        rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = Snoc(snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsSnoc(a +: CNil, snoc)

    override def size: Int = snoc.size
  }

  private case class PSnoc[E](pre: ConsList[ConsCarry[E]], snoc: SnocList[E]) extends InceptionQ[E] {
    override def size: Int = pre.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PASnoc(pre                    , SNil :+ Snoc(snoc),                    SNil :+ s)
//      case Cons      (c         ) => PA    (pre                    , SNil :+ Snoc(snoc) :+ rev(Cons(c)     )         )
//      case ConsP     (c, p      ) => PA    (pre                    , SNil :+ Snoc(snoc) :+ rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => PA    (pre :>: rev(Snoc(snoc)),                       rev(Cons(c)     ) :<: a   )
//      case ConsPA    (c, p, a   ) => PA    (pre :>: rev(Snoc(snoc)),                       rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => PASnoc(pre                    , SNil :+ Snoc(snoc) :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => PASnoc(pre :>: rev(Snoc(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => PASnoc(pre :>: rev(Snoc(snoc)),                       rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => PASnoc(pre :>: rev(Snoc(snoc)),                       rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => PA    (pre                    , SNil :+ Snoc(snoc) :+ rev(           p)         )
//      case A         (      a   ) => PA    (pre,                             Snoc(snoc) :<:                      a   )
//      case PA        (   p, a   ) => PA    (pre :>: rev(Snoc(snoc)),                       rev(           p) :<: a   )
//      case Snoc      (         s) => PASnoc(pre :>: rev(Snoc(snoc)), SNil :+ Snoc(snoc)                           , s)
//      case PSnoc     (   p,    s) => PASnoc(pre :>: rev(Snoc(snoc)), SNil :+               rev(           p)      , s)
//      case ASnoc     (      a, s) => PASnoc(pre :>: rev(Snoc(snoc)),                                             a, s)
//      case PASnoc    (   p, a, s) => PASnoc(pre :>: rev(Snoc(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = PSnoc(pre, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPSnoc(a +: CNil, pre, snoc)
  }

  private case class ASnoc[E](app: SnocList[SnocCarry[E]], snoc: SnocList[E]) extends InceptionQ[E] {
    override def size: Int = app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => ASnoc (                                app,                                   snoc :+ s)
//      case Cons      (c         ) => PA    (rev(app              ) +: CNil, SNil :+ Snoc(snoc) :+ rev(Cons(c)     )         )
//      case ConsP     (c, p      ) => PA    (rev(app :+ Snoc(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => PA    (rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a   )
//      case ConsPA    (c, p, a   ) => PA    (rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => PASnoc(rev(app              ) +: CNil, SNil :+ Snoc(snoc) :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => PASnoc(rev(app :+ Snoc(snoc)) +: CNil, SNil :+               rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => PASnoc(rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c)     ) :<: a, s)
//      case ConsPASnoc(c, p, a, s) => PASnoc(rev(app :+ Snoc(snoc)) +: CNil,                       rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => PA    (rev(app              ) +: CNil, SNil :+ Snoc(snoc) :+ rev(           p)         )
//      case A         (      a   ) => PA    (rev(app              ) +: CNil,         Snoc(snoc)                      :<: a   )
//      case PA        (   p, a   ) => PA    (rev(app :+ Snoc(snoc)) +: CNil,                       rev(           p) :<: a   )
//      case Snoc      (         s) => ASnoc (                                app  :+ Snoc(snoc)                           , s)
//      case PSnoc     (   p,    s) => PASnoc(rev(app :+ Snoc(snoc)) +: CNil, SNil :+               rev(           p)      , s)
//      case ASnoc     (      a, s) => PASnoc(rev(app              ) +: CNil,         Snoc(snoc)                      :<: a, s)
//      case PASnoc    (   p, a, s) => PASnoc(rev(app              ) +: CNil,                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = ASnoc(app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsASnoc(a +: CNil, app, snoc)
  }

  private case class PASnoc[E](pre: ConsList[ConsCarry[E]], app: SnocList[SnocCarry[E]], snoc: SnocList[E]) extends InceptionQ[E] {
    override def size: Int = pre.carrySize + app.carrySize + snoc.size

    override def isEmpty: Boolean = false

    override def ++(rhs: InceptionQ[E]): InceptionQ[E] = rhs match {
      case Unity     (         s) => PASnoc(pre,                            app,                                   snoc :+ s)
//      case c@Cons    (_         ) => PA    (pre :>: rev(app              ), SNil :+ Snoc(snoc) :+ rev(     c      )         )
//      case ConsP     (c, p      ) => PA    (pre :>: rev(app :+ Snoc(snoc)), SNil :+               rev(Cons(c) +: p)         )
//      case ConsA     (c,    a   ) => PA    (pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c))      :<: a   )
//      case ConsPA    (c, p, a   ) => PA    (pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c) +: p) :<: a   )
//      case ConsSnoc  (c,       s) => PASnoc(pre :>: rev(app              ), SNil :+ Snoc(snoc) :+ rev(Cons(c)     )      , s)
//      case ConsPSnoc (c, p,    s) => PASnoc(pre :>: rev(app :+ Snoc(snoc)), SNil :+               rev(Cons(c) +: p)      , s)
//      case ConsASnoc (c,    a, s) => PASnoc(pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c))      :<: a, s)
//      case ConsPASnoc(c, p, a, s) => PASnoc(pre :>: rev(app :+ Snoc(snoc)),                       rev(Cons(c) +: p) :<: a, s)
//      case QNil()                   => this
//      case P         (   p      ) => PA    (pre                           , app  :+ Snoc(snoc) :+ rev(           p)         )
//      case A         (      a   ) => PA    (pre :>: rev(app              ),         Snoc(snoc)                      :<: a   )
//      case PA        (   p, a   ) => PA    (pre :>: rev(app :+ Snoc(snoc)),                       rev(           p) :<: a   )
//      case Snoc      (         s) => PASnoc(pre                           , app  :+ Snoc(snoc)                           , s)
//      case PSnoc     (   p,    s) => PASnoc(pre                           , app  :+ Snoc(snoc) :+ rev(           p)      , s)
//      case ASnoc     (      a, s) => PASnoc(pre :>: rev(app :+ Snoc(snoc)),                                             a, s)
//      case PASnoc    (   p, a, s) => PASnoc(pre :>: rev(app :+ Snoc(snoc)),                       rev(           p) :<: a, s)
    }

    override def :+(a: E): InceptionQ[E] = PASnoc(pre, app, snoc :+ a)

    override def +:(a: E): InceptionQ[E] = ConsPASnoc(a +: CNil, pre, app, snoc)
  }
}