package scolls

import java.nio.ByteBuffer

import org.scalameter.api._
import org.scalameter.picklers.{IntPickler, PrimitivePickler}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object SeqBenchmarks
  extends Bench.OfflineReport {

  import StdlibInstances._

  implicit val intPickler = IntPickler

  implicit val twoIntPickler = new PrimitivePickler[(Int, Int)] {
    override protected def bits: Int = java.lang.Integer.SIZE * 2
    override protected def unwrap(from: ByteBuffer): (Int, Int) = (from.getInt(), from.getInt())
    override def pickle(x: (Int, Int)): Array[Byte] = byteBuffer.putInt(x._1).putInt(x._2).array()
  }

  val constructSizes: Gen[Int] = Gen.enumeration("sizes")(10, 100, 1000, 2000)
  val destructSizes: Gen[Int] = Gen.enumeration("sizes")(100, 1000)
  val concatInnerOuterSizes: Gen[(Int, Int)] = Gen.enumeration("sizes")((100, 10), (200, 30))
  val treeSizes: Gen[Int] = Gen.enumeration("treeSizes")(100, 500, 2000)
  val balancedTreeSizes: Gen[Int] = Gen.enumeration("treeSizes")(15, 20)
  val leftNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateLeftNestedTree)
  val rightNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateRightNestedTree)
  val jaggedNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateJaggedTree)
  val balancedNestedTrees: Gen[LTree[Int]] = balancedTreeSizes.map(generateBalancedTree)
  def consStructsToConcat[S[_]: Sequence]: Gen[List[S[Int]]] =
    for {
      innerOuterSizes <- concatInnerOuterSizes
      (innerSize, outerSize) = innerOuterSizes
      innerSeq = consRecBench[S](innerSize)
      outerSeq = List.fill(outerSize)(innerSeq)
    } yield outerSeq
  def snocStructsToConcat[S[_]: Sequence]: Gen[List[S[Int]]] =
    for {
      innerOuterSizes <- concatInnerOuterSizes
      (innerSize, outerSize) = innerOuterSizes
      innerSeq = snocRecBench[S](innerSize)
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
    seq = consRecBench[S](size)
  } yield seq

  def testSnocSeqOnes[S[_] : Sequence]: Gen[S[Int]] = for {
    size <- destructSizes
    seq = snocRecBench[S](size)
  } yield seq

  import KaplanTarjanSteq.Steq

  def consRecBench[S[_]](size: Int)(implicit S: Sequence[S]): S[Int] = {
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

  def snocRecBench[S[_]](size: Int)(implicit S: Sequence[S]): S[Int] = {
    @annotation.tailrec
    def loop(s: S[Int], cnt: Int): S[Int] = {
      if (cnt == 0)
        s
       else
        loop(S.snoc(s, cnt), cnt - 1)
    }
    loop(S.empty[Int], size)
  }

  def sumBench[S[_]](seq: S[Int])(implicit S: Sequence[S]): Int =
    S.fold(seq)(0)(_ + _)

  def concatBenchRight[S[_]](seq: List[S[Int]])(implicit S: CSequence[S]): Int =
    sumBench[S](seq.reduceRight(S.concat))(S.sequence)

  def concatBenchLeft[S[_]](seq: List[S[Int]])(implicit S: CSequence[S]): Int =
    sumBench[S](seq.reduceLeft(S.concat))(S.sequence)

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

  performance of "cons" in {
    measure method "list" in (using(constructSizes) in consRecBench[List])
    measure method "steq" in (using(constructSizes) in consRecBench[Steq])
    measure method "vec" in (using(constructSizes) in consRecBench[Vector])
    measure method "que" in (using(constructSizes) in consRecBench[OkasakiQueue])
    measure method "hque" in (using(constructSizes) in consRecBench[HQueue])
    measure method "cat" in (using(constructSizes) in consRecBench[Catenable])
    measure method "sque" in (using(constructSizes) in consRecBench[Queue])
    measure method "stream" in (using(constructSizes) in consRecBench[Stream])
  }

  performance of "snoc" in {
    measure method "list" in (using(constructSizes) in snocRecBench[List])
    measure method "steq" in (using(constructSizes) in snocRecBench[Steq])
    measure method "vec" in (using(constructSizes) in snocRecBench[Vector])
    measure method "que" in (using(constructSizes) in snocRecBench[OkasakiQueue])
    measure method "hque" in (using(constructSizes) in snocRecBench[HQueue])
    measure method "cat" in (using(constructSizes) in snocRecBench[Catenable])
    measure method "sque" in (using(constructSizes) in snocRecBench[Queue])
    measure method "stream" in (using(constructSizes) in snocRecBench[Stream])
  }

  performance of "summing cons-constructed seq" in {
    measure method "list" in (using(testConsSeqOnes[List]) in sumBench[List])
    measure method "steq" in (using(testConsSeqOnes[Steq]) in sumBench[Steq])
    measure method "vec" in (using(testConsSeqOnes[Vector]) in sumBench[Vector])
    measure method "que" in (using(testConsSeqOnes[OkasakiQueue]) in sumBench[OkasakiQueue])
    measure method "hque" in (using(testConsSeqOnes[HQueue]) in sumBench[HQueue])
    measure method "cat" in (using(testConsSeqOnes[Catenable]) in sumBench[Catenable])
    measure method "sque" in (using(testConsSeqOnes[Queue]) in sumBench[Queue])
    measure method "stream" in (using(testConsSeqOnes[Stream]) in sumBench[Stream])
  }

  performance of "summing snoc-constructed seq" in {
    measure method "list" in (using(testSnocSeqOnes[List]) in sumBench[List])
    measure method "steq" in (using(testSnocSeqOnes[Steq]) in sumBench[Steq])
    measure method "vec" in (using(testSnocSeqOnes[Vector]) in sumBench[Vector])
    measure method "que" in (using(testSnocSeqOnes[OkasakiQueue]) in sumBench[OkasakiQueue])
    measure method "hque" in (using(testSnocSeqOnes[HQueue]) in sumBench[HQueue])
    measure method "cat" in (using(testSnocSeqOnes[Catenable]) in sumBench[Catenable])
    measure method "sque" in (using(testSnocSeqOnes[Queue]) in sumBench[Queue])
    measure method "stream" in (using(testSnocSeqOnes[Stream]) in sumBench[Stream])
  }

  performance of "concatenating and summing cons-constructed seqs to the right" in {
    measure method "list" in (using(consStructsToConcat[List]) in concatBenchRight[List])
    measure method "vec" in (using(consStructsToConcat[Vector]) in concatBenchRight[Vector])
    measure method "que" in (using(consStructsToConcat[OkasakiQueue]) in concatBenchRight[OkasakiQueue])
    measure method "hque" in (using(consStructsToConcat[HQueue]) in concatBenchRight[HQueue])
    measure method "cat" in (using(consStructsToConcat[Catenable]) in concatBenchRight[Catenable])
    measure method "sque" in (using(consStructsToConcat[Queue]) in concatBenchRight[Queue])
    measure method "stream" in (using(consStructsToConcat[Stream]) in concatBenchRight[Stream])
  }

  performance of "concatenating and summing snoc-constructed seqs to the right" in {
    measure method "list" in (using(snocStructsToConcat[List]) in concatBenchRight[List])
    measure method "vec" in (using(snocStructsToConcat[Vector]) in concatBenchRight[Vector])
    measure method "que" in (using(snocStructsToConcat[OkasakiQueue]) in concatBenchRight[OkasakiQueue])
    measure method "hque" in (using(snocStructsToConcat[HQueue]) in concatBenchRight[HQueue])
    measure method "cat" in (using(snocStructsToConcat[Catenable]) in concatBenchRight[Catenable])
    measure method "sque" in (using(snocStructsToConcat[Queue]) in concatBenchRight[Queue])
    measure method "stream" in (using(snocStructsToConcat[Stream]) in concatBenchRight[Stream])
  }

  performance of "concatenating and summing cons-constructed seqs to the left" in {
    measure method "list" in (using(consStructsToConcat[List]) in concatBenchLeft[List])
    measure method "vec" in (using(consStructsToConcat[Vector]) in concatBenchLeft[Vector])
    measure method "que" in (using(consStructsToConcat[OkasakiQueue]) in concatBenchLeft[OkasakiQueue])
    measure method "hque" in (using(consStructsToConcat[HQueue]) in concatBenchLeft[HQueue])
    measure method "cat" in (using(consStructsToConcat[Catenable]) in concatBenchLeft[Catenable])
    measure method "sque" in (using(consStructsToConcat[Queue]) in concatBenchLeft[Queue])
    measure method "stream" in (using(consStructsToConcat[Stream]) in concatBenchLeft[Stream])
  }

  performance of "concatenating and summing snoc-constructed seqs to the left" in {
    measure method "list" in (using(snocStructsToConcat[List]) in concatBenchLeft[List])
    measure method "vec" in (using(snocStructsToConcat[Vector]) in concatBenchLeft[Vector])
    measure method "que" in (using(snocStructsToConcat[OkasakiQueue]) in concatBenchLeft[OkasakiQueue])
    measure method "hque" in (using(snocStructsToConcat[HQueue]) in concatBenchLeft[HQueue])
    measure method "cat" in (using(snocStructsToConcat[Catenable]) in concatBenchLeft[Catenable])
    measure method "sque" in (using(snocStructsToConcat[Queue]) in concatBenchLeft[Queue])
    measure method "stream" in (using(snocStructsToConcat[Stream]) in concatBenchLeft[Stream])
  }

  performance of "concatenating left-nested trees" in {
    measure method "list" in (using(leftNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(leftNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(leftNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(leftNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(leftNestedTrees) in frontierRec[Catenable, Int])
    measure method "sque" in (using(leftNestedTrees) in frontierRec[Queue, Int])
    measure method "stream" in (using(leftNestedTrees) in frontierRec[Stream, Int])
  }

  performance of "concatenating right-nested trees" in {
    measure method "list" in (using(rightNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(rightNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(rightNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(rightNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(rightNestedTrees) in frontierRec[Catenable, Int])
    measure method "sque" in (using(rightNestedTrees) in frontierRec[Queue, Int])
    measure method "stream" in (using(rightNestedTrees) in frontierRec[Stream, Int])
  }

  performance of "concatenating jagged trees" in {
    measure method "list" in (using(jaggedNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(jaggedNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(jaggedNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(jaggedNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(jaggedNestedTrees) in frontierRec[Catenable, Int])
    measure method "sque" in (using(jaggedNestedTrees) in frontierRec[Queue, Int])
    measure method "stream" in (using(jaggedNestedTrees) in frontierRec[Stream, Int])
  }

  performance of "concatenating balanced trees" in {
    measure method "list" in (using(balancedNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(balancedNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(balancedNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(balancedNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(balancedNestedTrees) in frontierRec[Catenable, Int])
    measure method "sque" in (using(balancedNestedTrees) in frontierRec[Queue, Int])
    measure method "stream" in (using(balancedNestedTrees) in frontierRec[Stream, Int])
  }

  performance of "functionally concatenating left-nested trees" in {
    measure method "list" in (using(leftNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(leftNestedTrees) in frontierCPS[Vector, Int])
    measure method "que" in (using(leftNestedTrees) in frontierCPS[OkasakiQueue, Int])
    measure method "hque" in (using(leftNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(leftNestedTrees) in frontierCPS[Catenable, Int])
    measure method "sque" in (using(leftNestedTrees) in frontierCPS[Queue, Int])
    measure method "stream" in (using(leftNestedTrees) in frontierCPS[Stream, Int])
  }

  performance of "functionally concatenating right-nested trees" in {
    measure method "list" in (using(rightNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(rightNestedTrees) in frontierCPS[Vector, Int])
    measure method "que" in (using(rightNestedTrees) in frontierCPS[OkasakiQueue, Int])
    measure method "hque" in (using(rightNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(rightNestedTrees) in frontierCPS[Catenable, Int])
    measure method "sque" in (using(rightNestedTrees) in frontierCPS[Queue, Int])
    measure method "stream" in (using(rightNestedTrees) in frontierCPS[Stream, Int])
  }

  performance of "functionally concatenating jagged trees" in {
    measure method "list" in (using(jaggedNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(jaggedNestedTrees) in frontierCPS[Vector, Int])
    measure method "que" in (using(jaggedNestedTrees) in frontierCPS[OkasakiQueue, Int])
    measure method "hque" in (using(jaggedNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(jaggedNestedTrees) in frontierCPS[Catenable, Int])
    measure method "sque" in (using(jaggedNestedTrees) in frontierCPS[Queue, Int])
    measure method "stream" in (using(jaggedNestedTrees) in frontierCPS[Stream, Int])
  }

  performance of "functionally concatenating balanced trees" in {
    measure method "list" in (using(balancedNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(balancedNestedTrees) in frontierCPS[Vector, Int])
    measure method "que" in (using(balancedNestedTrees) in frontierCPS[OkasakiQueue, Int])
    measure method "hque" in (using(balancedNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(balancedNestedTrees) in frontierCPS[Catenable, Int])
    measure method "sque" in (using(balancedNestedTrees) in frontierCPS[Queue, Int])
    measure method "stream" in (using(balancedNestedTrees) in frontierCPS[Stream, Int])
  }


}
