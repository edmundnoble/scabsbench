package scabs
package seq

import java.nio.ByteBuffer

import org.scalameter.api._
import org.scalameter.picklers.{IntPickler, PrimitivePickler}

import scala.annotation.tailrec

object Manipulators {

  implicit val intPickler = IntPickler

  implicit val twoIntPickler = new PrimitivePickler[(Int, Int)] {
    override protected def bits: Int = java.lang.Integer.SIZE * 2
    override protected def unwrap(from: ByteBuffer): (Int, Int) = (from.getInt(), from.getInt())
    override def pickle(x: (Int, Int)): Array[Byte] = byteBuffer.putInt(x._1).putInt(x._2).array()
  }

  val constructSizes: Gen[Int] = Gen.enumeration("constructSizes")(10, 100, 2000, 3000)
  val queueBenchSizes: Gen[Int] = Gen.enumeration("queueBenchSizes")(6000)
  val stackBenchSizes: Gen[Int] = Gen.enumeration("stackBenchSizes")(6000)
  val destructSizes: Gen[Int] = Gen.enumeration("destructSizes")(1500, 3000)
  val concatInnerOuterSizes: Gen[(Int, Int)] = Gen.enumeration("concatInnerOuterSizes")((300, 30), (500, 50))
  val treeSizes: Gen[Int] = Gen.enumeration("treeSizes")(500, 1000, 2000)
  val balancedTreeSizes: Gen[Int] = Gen.enumeration("balancedTreeSizes")(16, 19)
  val leftNestedTrees: Gen[LTree[Int]] = treeSizes.map(TreeManipulators.generateLeftNestedTree)
  val rightNestedTrees: Gen[LTree[Int]] = treeSizes.map(TreeManipulators.generateRightNestedTree)
  val jaggedNestedTrees: Gen[LTree[Int]] = treeSizes.map(TreeManipulators.generateJaggedTree)
  val balancedNestedTrees: Gen[LTree[Int]] = balancedTreeSizes.map(TreeManipulators.generateBalancedTree)

  def consStructsToConcat[S[_] : Sequence]: Gen[List[S[Int]]] =
    for {
      innerOuterSizes <- concatInnerOuterSizes
      (innerSize, outerSize) = innerOuterSizes
      innerSeq = consRec[S](innerSize)
      outerSeq = List.fill(outerSize)(innerSeq)
    } yield outerSeq
  def snocStructsToConcat[S[_] : Sequence]: Gen[List[S[Int]]] =
    for {
      innerOuterSizes <- concatInnerOuterSizes
      (innerSize, outerSize) = innerOuterSizes
      innerSeq = snocRec[S](innerSize)
      outerSeq = List.fill(outerSize)(innerSeq)
    } yield outerSeq

  def testConsSeqOnes[S[_]: Sequence]: Gen[S[Int]] = for {
    size <- destructSizes
    seq = consRec[S](size)
  } yield seq

  def testSnocSeqOnes[S[_] : Sequence]: Gen[S[Int]] = for {
    size <- destructSizes
    seq = snocRec[S](size)
  } yield seq

  def consRec[S[_]](size: Int)(implicit S: Sequence[S]): S[Int] = {
    @annotation.tailrec
    def loop(s: S[Int], cnt: Int): S[Int] = {
      if (cnt == 0) {
        s
      } else {
        loop(S.cons(cnt, s), cnt - 1)
      }
    }
    loop(S.empty[Int], size)
  }

  def snocRec[S[_]](size: Int)(implicit S: Sequence[S]): S[Int] = {
    @annotation.tailrec
    def loop(s: S[Int], cnt: Int): S[Int] = {
      if (cnt == 0)
        s
      else
        loop(S.snoc(s, cnt), cnt - 1)
    }
    loop(S.empty[Int], size)
  }

  def sum[S[_]](seq: S[Int])(implicit S: Sequence[S]): Int =
    S.fold(seq)(0)(_ + _)

  def concatRight[S[_]](seq: List[S[Int]])(implicit S: Sequence[S]): Int =
    sum[S](seq.reduceRight(S.concat))

  def concatLeft[S[_]](seq: List[S[Int]])(implicit S: Sequence[S]): Int =
    sum[S](seq.reduceLeft(S.concat))

  def frontierRec[S[_], A](tree: LTree[A])(implicit S: Sequence[S]): S[A] =
    tree match {
      case Lf(a) => S.one(a)
      case Bin(_, left, right) => S.concat(frontierRec[S, A](left), frontierRec[S, A](right))
    }

  @tailrec
  def queue[S[_]](start: S[Int], size: Int)(implicit S: Sequence[S]): S[Int] =
    if (size == 0) start
    else queue[S](S.tail(S.snoc(start, size)), size - 1)

  @tailrec
  def stack[S[_]](start: S[Int], size: Int)(implicit S: Sequence[S]): S[Int] =
    if (size == 0) start
    else stack[S](S.tail(S.cons(size, start)), size - 1)

  def noop[S[_]](seq: S[Int])(implicit S: Sequence[S]): Unit = {
    // do nothing -- just testing building speed
  }

}
