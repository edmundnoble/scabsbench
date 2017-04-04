package scabs
package seq

import java.nio.ByteBuffer

import org.scalameter.api._
import org.scalameter.picklers.{IntPickler, PrimitivePickler}
import StdlibInstances._
import scabs.Util.Id
import scabs.seq.Hierarchy._

import scala.collection.immutable.Queue

object Benchmarks extends java.io.Serializable {

  implicit val intPickler = IntPickler

  implicit val twoIntPickler = new PrimitivePickler[(Int, Int)] {
    protected def bits: Int = java.lang.Integer.SIZE * 2
    protected def unwrap(from: ByteBuffer): (Int, Int) = (from.getInt(), from.getInt())
    def pickle(x: (Int, Int)): Array[Byte] = byteBuffer.putInt(x._1).putInt(x._2).array()
  }

  def sequenceVarieties = Seq(
    TCBenchVariety[Sequence, List]("list").forget,
    TCBenchVariety[Sequence, Vector]("vec").forget,
    TCBenchVariety[Sequence, Queue]("que").forget,
    TCBenchVariety[Sequence, Catenable]("cat").forget,
    TCBenchVariety[Sequence, CatenableArrLeaves]("catarr").forget,
    TCBenchVariety[Sequence, TurtleQ]("turtle").forget
  )

  import Manipulators._

  def sumConsBench = new TCBenchmark[Sequence, Id, Int] {
    def name = "summing cons-constructed seq"
    def run[F[_] : Sequence]: F[Int] => Any = sum[F]
    def gen[F[_] : Sequence]: Gen[F[Int]] = testConsSeqOnes[F]
  }

  def sumSnocBench = new TCBenchmark[Sequence, Id, Int] {
    def name: String = "summing snoc-constructed seq"
    def run[F[_] : Sequence]: F[Int] => Any = sum[F]
    def gen[F[_] : Sequence]: Gen[F[Int]] = testSnocSeqOnes[F]
  }

  def queueBench = new TCBenchmark[Sequence, (?, Int), Int] {
    def name: String = "queueing (alternating snoc and tail)"
    def run[F[_] : Sequence]: ((F[Int], Int)) => Any = (queue[F] _).tupled
    def gen[F[_] : Sequence]: Gen[(F[Int], Int)] = testSnocSeqOnes[F] zip queueBenchSizes
  }

  def stackBench = new TCBenchmark[Sequence, (?, Int), Int] {
    def name: String = "stack (alternating cons and tail)"
    def run[F[_] : Sequence]: ((F[Int], Int)) => Any = (stack[F] _).tupled
    def gen[F[_] : Sequence]: Gen[(F[Int], Int)] = testSnocSeqOnes[F] zip stackBenchSizes
  }

  def consConcatBenchRight = new TCBenchmark[Sequence, List, Int] {
    def name: String = "concatenating and summing cons-constructed seqs to the right"
    def run[F[_] : Sequence]: List[F[Int]] => Any = concatRight[F]
    def gen[F[_] : Sequence]: Gen[List[F[Int]]] = consStructsToConcat[F]
  }

  def snocConcatBenchRight = new TCBenchmark[Sequence, List, Int] {
    def name: String = "concatenating and summing snoc-constructed seqs to the right"
    def run[F[_] : Sequence]: List[F[Int]] => Any = concatRight[F]
    def gen[F[_] : Sequence]: Gen[List[F[Int]]] = snocStructsToConcat[F]
  }

  def consConcatBenchLeft = new TCBenchmark[Sequence, List, Int] {
    def name: String = "concatenating and summing cons-constructed seqs to the left"
    def run[F[_] : Sequence]: List[F[Int]] => Any = concatLeft[F]
    def gen[F[_] : Sequence]: Gen[List[F[Int]]] = consStructsToConcat[F]
  }

  def snocConcatBenchLeft = new TCBenchmark[Sequence, List, Int] {
    def name: String = "concatenating and summing snoc-constructed seqs to the left"
    def run[F[_] : Sequence]: List[F[Int]] => Any = concatLeft[F]
    def gen[F[_] : Sequence]: Gen[List[F[Int]]] = snocStructsToConcat[F]
  }

  def concatLeftNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing left-nested trees", leftNestedTrees) {
      def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int] _ andThen sum[F]
    }

  def concatRightNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing right-nested trees", rightNestedTrees) {
      def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int] _ andThen sum[F]
    }

  def concatJaggedNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing jagged trees", jaggedNestedTrees) {
      def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int] _ andThen sum[F]
    }

  def concatBalancedNestedBench =
    new ConstTCBenchmark[Sequence, LTree[Int]]("concatenating and summing balanced trees", balancedNestedTrees) {
      def run[F[_] : Sequence]: LTree[Int] => Any = frontierRec[F, Int] _ andThen sum[F]
    }

  def allSeqBenchmarks: Seq[TCBenchmark[Sequence, Nothing, Nothing]] = Seq(
    sumConsBench.forget, sumSnocBench.forget,
    queueBench.forget, stackBench.forget,
    consConcatBenchLeft.forget, consConcatBenchRight.forget,
    snocConcatBenchLeft.forget, snocConcatBenchRight.forget,
    concatLeftNestedBench.forget, concatRightNestedBench.forget,
    concatJaggedNestedBench.forget,
    concatBalancedNestedBench.forget
  )

  val seqBenchSuite: TCBenchSuite[Sequence] =
    TCBenchSuite(sequenceVarieties, allSeqBenchmarks)
}
