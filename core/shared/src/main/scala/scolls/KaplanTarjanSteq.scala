package scolls

object KaplanTarjanSteq {
  case class Steq[A](cons: List[C[A]])

  sealed trait C[A]
  case class PS[A](ps: PST[A]) extends C[A]
  case class Y[A](ps: List[PST[A]]) extends C[A]

  type PST[A] = (List[LTree[A]], List[LTree[A]])

  sealed trait Color
  case object Red extends Color
  case object Yellow extends Color
  case object Green extends Color

  def empty[A]: Steq[A] = Steq(Nil)
  def isEmpty[A](steq: Steq[A]): Boolean = steq.cons.isEmpty
  def nonEmpty[A](steq: Steq[A]): Boolean = steq.cons.nonEmpty
  def head[A](steq: Steq[A]): A = {
    val (ps, _) = popPS(steq.cons)
    headPS(ps) match {
      case Lf(a) => a
    }
  }
  def last[A](steq: Steq[A]): A = {
    val (ps, _) = popPS(steq.cons)
    lastPS(ps) match {
      case Lf(a) => a
    }
  }
  def cons[A](a: A, steq: Steq[A]): Steq[A] = if (nonEmpty(steq)) {
    val ((px, sx), cs1) = popPS(steq.cons)
    Steq[A](normD(fixY(PS(Lf(a) :: px, sx) :: cs1)))
  } else {
    Steq[A](PS[A](Lf[A](a) :: Nil, Nil) :: Nil)
  }
  def snoc[A](steq: Steq[A], a: A): Steq[A] = if (nonEmpty(steq)) {
    val ((px, sx), cs1) = popPS(steq.cons)
    Steq[A](normD(fixY(PS(Lf(a) :: px, sx) :: cs1)))
  } else {
    Steq[A](PS[A](Nil, Lf[A](a) :: Nil) :: Nil)
  }
  def tail[A](steq: Steq[A]): Steq[A] = {
    val (ps, cs1) = popPS(steq.cons)
    Steq[A](normD(fixY[A](PS[A](tailPS[A](ps)) :: cs1)))
  }
  def init[A](steq: Steq[A]): Steq[A] = {
    val (ps, cs1) = popPS(steq.cons)
    Steq[A](normD(fixY(PS(initPS(ps)) :: cs1)))
  }
  def length[A](steq: Steq[A]): Int = {
    def accSum(c: C[A], soFar: Int): Int = c match {
      case PS((px, sx)) => px.length + sx.length + 2 * soFar
      case Y(pss) => pss.map(PS[A]).foldRight(soFar)(accSum)
    }
    steq.cons.foldRight(0)(accSum)
  }
  def toList[A](steq: Steq[A]): List[A] = {
    def toListC(c: C[A], rest: List[A]): List[A] = c match {
      case PS(ps) => toListPS(ps, rest)
      case Y(pss) => pss.foldRight(rest)(toListPS)
    }
    steq.cons.foldRight(Nil: List[A])(toListC)
  }
  def popPS[A](cons: List[C[A]]): (PST[A], List[C[A]]) = {
    val head = cons.head
    val tail = cons.tail
    head match {
      case PS(ps) => (ps, tail)
      case Y(List(ps)) => (ps, tail)
      case Y(ps :: pss) => (ps, Y(pss) :: tail)
    }
  }

  def normD[A](cons: List[C[A]]): List[C[A]] = {
    if (cons.isEmpty) cons
    else colorOfFirst(cons) match {
      case Red => fix(cons.head, cons.tail)
      case Yellow => cons.head :: normD[A](cons.tail)
      case Green => cons
    }
  }

  def fix[A](con: C[A], cons: List[C[A]]): List[C[A]] = {
    (con, cons) match {
      case (PS((px, sx)), Nil) =>
        val n = px.length + sx.length
        if (n == 0) Nil
        else if (n >= 6) PS((px ++ sx).splitAt(3)) :: Nil
        else {
          val (psn, ps1) = makeGrPS[A]((px, sx), (Nil, Nil))
          PS[A](psn) :: fixY[A](PS[A](ps1) :: Nil)
        }
      case (PS(ps), cs) =>
        val (ps1, cs1) = popPS(cs)
        makeGrPS(ps, ps1) match {
          case (psn, (Nil, Nil)) => fixY[A](PS[A](psn) :: regular[A](PS[A]((Nil, Nil)) :: cs1))
          case (psn, ps1n) => PS(ps) :: fixY(PS(ps1) :: cs1)
        }
    }
  }

  def min(fst: Color, snd: Color): Color =
    if (fst == Red) fst
    else if (fst == Green) snd
    else if (snd == Red) snd
    else if (snd == Green) fst
    else Yellow

  def fixY[A](cons: List[C[A]]): List[C[A]] = {
    val ps = cons.head.asInstanceOf[PS[A]].ps
    if (cons.length == 1) {
      val ps = cons.head.asInstanceOf[PS[A]].ps
      def isYellow(xs: PST[A]): Boolean = xs match {
        case (Nil, sx) => colorX(sx) == Yellow
        case (px, Nil) => colorX(px) == Yellow
        case (px, sx) => min(colorX(px), colorX(sx)) == Yellow
      }
      if (isYellow(ps)) Y(ps :: Nil) :: Nil else cons
    } else {
      def isYellow(ps: PST[A]): Boolean = min(colorX(ps._1), colorX(ps._2)) == Yellow
      val cs = cons.tail
      if (isYellow(ps)) cs match {
        case (Y(pss) :: cs1) => Y(ps :: pss) :: cs1
        case _ => Y(ps :: Nil) :: cs
      } else {
        cons
      }
    }
  }

    def regular[A](cons: List[C[A]]): List[C[A]] = cons match {
      case PS((Nil, Nil)) :: Nil => Nil
      case _ => cons
    }

    def colorOfFirst[A](cons: List[C[A]]): Color = cons match {
      case Y(_) :: _ => Yellow
      case PS((Nil, sx)) :: Nil => colorX(sx)
      case PS((px, Nil)) :: Nil => colorX(px)
      case PS((px, sx)) :: _ => min(colorX(px), colorX(sx))
    }

    def makeGrPS[A](t: PST[A], ps1: PST[A]): (PST[A], PST[A]) = {
      val (pxn, ps1n) = makeGrPx(t._1, ps1)
      val (sxn, ps1nn) = makeGrSx(t._2, ps1n)
      ((pxn, sxn), ps1nn)
    }

  def makeGrPx[A](px: List[LTree[A]], ps: PST[A]): (List[LTree[A]], PST[A]) = {
    val n = px.length
    if (n >= 4) {
      val (pxn, List(a1, a2)) = px.splitAt(n - 2)
      (pxn, (LTree.join(a1, a2) :: ps._1, ps._2))
    } else if (n <= 1) {
      val Bin(_, a1, a2) = headPS(ps)
      (px ++ List(a1, a2), tailPS(ps))
    } else {
      (px, ps)
    }
  }

  def makeGrSx[A](sx: List[LTree[A]], ps: PST[A]): (List[LTree[A]], PST[A]) = {
    val n = sx.length
    if (n >= 4) {
      val (a1 :: a2 :: sxn) = sx
      (sxn, (ps._1, ps._2 ++ List(LTree.join(a1, a2))))
    } else if (n <= 1 && nonEmptyPS(ps)) {
      val Bin(_, a1, a2) = lastPS(ps)
      (a1 :: a2 :: sx, initPS(ps))
    } else {
      (sx, ps)
    }
  }

  def nonEmptyPS[A](ps: PST[A]): Boolean = {
    ps._1 == Nil && ps._2 == Nil
  }

  def colorX[A](trees: List[LTree[A]]): Color = {
    val n = trees.length
    if (n == 2 || n == 3) Green
    else if (n == 1 || n == 4) Yellow
    else Red
  }

  def toListPS[A](t: PST[A], rest: List[A]): List[A] = {
    t._1.flatMap(LTree.frontier) ++ rest ++ t._2.flatMap(LTree.frontier)
  }

  def headPS[A](t: PST[A]): LTree[A] = {
    if (t._1.isEmpty) (t._2.head)
    else t._1.head
  }

  def lastPS[A](t: PST[A]): LTree[A] = {
    if (t._2.isEmpty) t._1.last
    else t._2.last
  }

  def tailPS[A](t: PST[A]): PST[A] = {
    if (t._1.isEmpty) (t._2.init, Nil)
    else (t._1.tail, t._2.init)
  }

  def initPS[A](t: PST[A]): PST[A] = {
    if (t._2.isEmpty) (t._1.init, Nil)
    else (t._1, t._2.init)
  }

}
