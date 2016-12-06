package scabs.colls

import java.nio.ByteBuffer

import org.scalameter.api._
import org.scalameter.picklers.{IntPickler, PrimitivePickler}

import scala.annotation.tailrec

object SeqManipulators {

  implicit val intPickler = IntPickler

  implicit val twoIntPickler = new PrimitivePickler[(Int, Int)] {
    override protected def bits: Int = java.lang.Integer.SIZE * 2
    override protected def unwrap(from: ByteBuffer): (Int, Int) = (from.getInt(), from.getInt())
    override def pickle(x: (Int, Int)): Array[Byte] = byteBuffer.putInt(x._1).putInt(x._2).array()
  }

  val constructSizes: Gen[Int] = Gen.enumeration("sizes")(10, 100, 1000, 2000)
  val queueBenchSizes: Gen[Int] = Gen.enumeration("sizes")(4000)
  val destructSizes: Gen[Int] = Gen.enumeration("sizes")(100, 1000, 2000)
  val concatInnerOuterSizes: Gen[(Int, Int)] = Gen.enumeration("sizes")((100, 10), (200, 30))
  val treeSizes: Gen[Int] = Gen.enumeration("treeSizes")(100, 500, 2000)
  val balancedTreeSizes: Gen[Int] = Gen.enumeration("treeSizes")(15, 20)
  val leftNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateLeftNestedTree)
  val rightNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateRightNestedTree)
  val jaggedNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateJaggedTree)
  val balancedNestedTrees: Gen[LTree[Int]] = balancedTreeSizes.map(generateBalancedTree)
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

  def generateLeftNestedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else {
        val newLeaf = Lf(cnt)
        generate(Bin(acc.size + 1, acc, newLeaf), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def generateRightNestedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else {
        val newLeaf = Lf(cnt)
        generate(Bin(acc.size + 1, newLeaf, acc), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def generateJaggedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else if (cnt % 2 == 0) {
        generate(Bin(acc.size + 1, Lf(cnt), acc), cnt - 1)
      } else {
        generate(Bin(acc.size + 1, acc, Lf(cnt)), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def generateBalancedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else {
        generate(Bin(acc.size * 2, acc, acc), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def testConsSeqOnes[S[_] : Sequence]: Gen[S[Int]] = for {
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

  def concatRight[S[_]](seq: List[S[Int]])(implicit S: CSequence[S]): Int =
    sum[S](seq.reduceRight(S.concat))(S.sequence)

  def concatLeft[S[_]](seq: List[S[Int]])(implicit S: CSequence[S]): Int =
    sum[S](seq.reduceLeft(S.concat))(S.sequence)

  def frontierCPS[S[_], A](tree: LTree[A])(implicit S: Sequence[S]): S[A] = {
    def helper(innerTree: LTree[A]): S[A] => S[A] = innerTree match {
      case Lf(a) => S.cons(a, _)
      case Bin(_, left, right) => helper(left) compose helper(right)
    }
    helper(tree)(S.empty)
  }

  def frontierRec[S[_], A](tree: LTree[A])(implicit S: CSequence[S]): S[A] =
    tree match {
      case Lf(a) => S.sequence.cons(a, S.sequence.empty)
      case Bin(_, left, right) => S.concat(frontierRec[S, A](left), frontierRec[S, A](right))
    }

  def queue[S[_]](start: S[Int], size: Int)(implicit S: Sequence[S]): S[Int] =
    if (size == 0) start
    else queue[S](S.tail(S.snoc(start, size)), size - 1)

}
