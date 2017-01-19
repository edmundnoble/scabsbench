package scabs.colls

import java.nio.ByteBuffer

import org.scalameter.api._
import org.scalameter.picklers.{IntPickler, PrimitivePickler}
import scabs.{ConstTCBenchmark, TCBenchSuite, TCBenchVariety, TCBenchmark}
import scabs.Util.Id
import scabs.colls.Hierarchy._

import scala.collection.immutable.Queue

object SeqBenchmarks extends java.io.Serializable {

  implicit def intPickler = IntPickler

  implicit def twoIntPickler = new PrimitivePickler[(Int, Int)] {
    override protected def bits: Int = java.lang.Integer.SIZE * 2
    override protected def unwrap(from: ByteBuffer): (Int, Int) = (from.getInt(), from.getInt())
    override def pickle(x: (Int, Int)): Array[Byte] = byteBuffer.putInt(x._1).putInt(x._2).array()
  }

  def sequenceVarieties = Seq(
    TCBenchVariety[Sequence, List]("list").forget,
    TCBenchVariety[Sequence, Vector]("vec").forget,
    TCBenchVariety[Sequence, OkasakiQueue]("que").forget,
    TCBenchVariety[Sequence, HQueue]("hque").forget,
    TCBenchVariety[Sequence, Catenable]("cat").forget,
    TCBenchVariety[Sequence, Queue]("sque").forget,
    TCBenchVariety[Sequence, Stream]("stream").forget
  )

  import SeqManipulators._

  def consRecBench = new ConstTCBenchmark[Sequence, Int]("cons", constructSizes) {
    override def run[F[_] : Sequence]: Int => Any = consRec[F]
  }

  def snocRecBench = new ConstTCBenchmark[Sequence, Int]("snoc", constructSizes) {
    override def run[F[_] : Sequence]: Int => Any = snocRec[F]
  }

  def sumConsBench = new TCBenchmark[Sequence, Id, Int] {
    override def name = "summing cons-constructed seq"
    override def run[F[_] : Sequence]: F[Int] => Any = sum[F]
    override def gen[F[_] : Sequence]: Gen[F[Int]] = testConsSeqOnes[F]
  }

  def sumSnocBench = new TCBenchmark[Sequence, Id, Int] {
    override def name: String = "summing snoc-constructed seq"
    override def run[F[_] : Sequence]: F[Int] => Any = sum[F]
    override def gen[F[_] : Sequence]: Gen[F[Int]] = testSnocSeqOnes[F]
  }

  def queueBench = new TCBenchmark[Sequence, (?, Int), Int] {
    override def name: String = "queueing (alternating snoc and tail)"
    override def run[F[_] : Sequence]: ((F[Int], Int)) => Any = (queue[F] _).tupled
    override def gen[F[_] : Sequence]: Gen[(F[Int], Int)] = testSnocSeqOnes[F] zip queueBenchSizes
  }

  def consConcatBenchRight = new TCBenchmark[Sequence, List, Int] {
    override def name: String = "concatenating and summing cons-constructed seqs to the right"
    override def run[F[_] : Sequence]: List[F[Int]] => Any = concatRight[F]
    override def gen[F[_] : Sequence]: Gen[List[F[Int]]] = consStructsToConcat[F]
  }

  def snocConcatBenchRight = new TCBenchmark[Sequence, List, Int] {
    override def name: String = "concatenating and summing snoc-constructed seqs to the right"
    override def run[F[_] : Sequence]: List[F[Int]] => Any = concatRight[F]
    override def gen[F[_] : Sequence]: Gen[List[F[Int]]] = snocStructsToConcat[F]
  }

  def consConcatBenchLeft = new TCBenchmark[Sequence, List, Int] {
    override def name: String = "concatenating and summing cons-constructed seqs to the left"
    override def run[F[_] : Sequence]: List[F[Int]] => Any = concatLeft[F]
    override def gen[F[_] : Sequence]: Gen[List[F[Int]]] = consStructsToConcat[F]
  }

  def snocConcatBenchLeft = new TCBenchmark[Sequence, List, Int] {
    override def name: String = "concatenating and summing snoc-constructed seqs to the left"
    override def run[F[_] : Sequence]: List[F[Int]] => Any = concatLeft[F]
    override def gen[F[_] : Sequence]: Gen[List[F[Int]]] = snocStructsToConcat[F]
  }

  def concatLeftNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing left-nested trees", leftNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int]
    }

  def concatRightNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing right-nested trees", rightNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int]
    }

  def concatJaggedNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing jagged trees", jaggedNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int]
    }

  def concatBalancedNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing balanced trees", balancedNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int]
    }

  def funConcatLeftNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("functionally concatenating and summing left-nested trees", leftNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierCPS[F, Int]
    }

  def funConcatRightNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("functionally concatenating and summing right-nested trees", rightNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierCPS[F, Int]
    }

  def funConcatJaggedNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("functionally concatenating jagged trees", jaggedNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierCPS[F, Int]
    }

  def funConcatBalancedNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("functionally concatenating balanced trees", balancedNestedTrees) {
      override def run[F[_] : Sequence]: LTree[Int] => Any = frontierCPS[F, Int]
    }

  def allSeqBenchmarks: Seq[TCBenchmark[Sequence, Nothing, Nothing]] = Seq(
    consRecBench.forget, snocRecBench.forget,
    sumConsBench.forget, sumSnocBench.forget,
    queueBench.forget,
    funConcatLeftNestedBench.forget, funConcatRightNestedBench.forget,
    funConcatJaggedNestedBench.forget,
    funConcatBalancedNestedBench.forget,
    consConcatBenchLeft.forget, consConcatBenchRight.forget,
    snocConcatBenchLeft.forget, snocConcatBenchRight.forget,
    concatLeftNestedBench.forget, concatRightNestedBench.forget,
    concatJaggedNestedBench.forget,
    concatBalancedNestedBench.forget
  )

  val seqBenchSuite: TCBenchSuite[Sequence] =
    TCBenchSuite(sequenceVarieties, allSeqBenchmarks)

  val benchSuites = Seq[TCBenchSuite[Nothing]](seqBenchSuite.forget)
}
