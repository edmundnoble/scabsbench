package scabs
package seq

import java.nio.ByteBuffer

import org.scalameter.api._
import org.scalameter.picklers.{IntPickler, PrimitivePickler}
import StdlibInstances._
import scabs.Util.Id

import scala.collection.immutable.Queue

object Benchmarks {

  implicit val intPickler = IntPickler

  implicit val twoIntPickler = new PrimitivePickler[(Int, Int)] {
    protected def bits: Int = java.lang.Integer.SIZE * 2

    protected def unwrap(from: ByteBuffer): (Int, Int) = (from.getInt(), from.getInt())

    def pickle(x: (Int, Int)): Array[Byte] = byteBuffer.putInt(x._1).putInt(x._2).array()
  }

  import Manipulators._

  val sumConsBench =
    new Benchmark.AuxC[Sequence, Id, Int]("summing cons-constructed seq") {
      def runBenchmark[Type[_] : Sequence](input: Type[Int]): Any = sum(input)

      def generateInput[Type[_] : Sequence]: Gen[Type[Int]] = testConsSeqOnes[Type]
    }

  val sumSnocBench =
    new Benchmark.AuxC[Sequence, Id, Int]("summing snoc-constructed seq") {
      def runBenchmark[Type[_] : Sequence](input: Type[Int]): Any = sum(input)

      def generateInput[Type[_] : Sequence]: Gen[Type[Int]] = testSnocSeqOnes[Type]
    }

  val queueBench =
    new Benchmark.AuxC[Sequence, (?, Int), Int]("queueing (alternating snoc and tail)") {
      def runBenchmark[Type[_] : Sequence](input: ((Type[Int], Int))): Any = Function.tupled(queue[Type] _)(input)

      def generateInput[Type[_] : Sequence]: Gen[(Type[Int], Int)] = testSnocSeqOnes[Type] zip queueBenchSizes
    }

  val stackBench =
    new Benchmark.AuxC[Sequence, (?, Int), Int]("stack (alternating cons and tail)") {
      def runBenchmark[Type[_] : Sequence](input: ((Type[Int], Int))): Any = Function.tupled(stack[Type] _)(input)

      def generateInput[Type[_] : Sequence]: Gen[(Type[Int], Int)] = testSnocSeqOnes[Type] zip stackBenchSizes
    }

  val consConcatBenchRight =
    new Benchmark.AuxC[Sequence, List, Int]("concatenating and summing cons-constructed seqs to the right") {
      def runBenchmark[Type[_] : Sequence](input: List[Type[Int]]): Any = concatRight(input)

      def generateInput[Type[_] : Sequence]: Gen[List[Type[Int]]] = consStructsToConcat[Type]
    }

  val snocConcatBenchRight =
    new Benchmark.AuxC[Sequence, List, Int]("concatenating and summing snoc-constructed seqs to the right") {
      def runBenchmark[Type[_] : Sequence](input: List[Type[Int]]): Any = concatRight(input)

      def generateInput[Type[_] : Sequence]: Gen[List[Type[Int]]] = snocStructsToConcat[Type]
    }

  val consConcatBenchLeft =
    new Benchmark.AuxC[Sequence, List, Int]("concatenating and summing cons-constructed seqs to the left") {
      def runBenchmark[Type[_] : Sequence](input: List[Type[Int]]): Any = concatLeft(input)

      def generateInput[Type[_] : Sequence]: Gen[List[Type[Int]]] = consStructsToConcat[Type]
    }

  val snocConcatBenchLeft =
    new Benchmark.AuxC[Sequence, List, Int]("concatenating and summing snoc-constructed seqs to the left") {
      def runBenchmark[Type[_] : Sequence](input: List[Type[Int]]): Any = concatLeft(input)

      def generateInput[Type[_] : Sequence]: Gen[List[Type[Int]]] = snocStructsToConcat[Type]
    }

  final case class ConcatAndSumTreeBenchmark(name0: String, input0: Gen[LTree[Int]])
    extends ConstantInputBenchmark[Sequence, LTree[Int]](name0, input0) {
    def runBenchmark[Type[_] : Sequence](input: LTree[Int]): Any = sum(frontierRec[Type, Int](input))
  }

  val concatLeftNestedBench =
    ConcatAndSumTreeBenchmark("concatenating and summing left-nested trees", leftNestedTrees)

  val concatRightNestedBench =
    ConcatAndSumTreeBenchmark("concatenating and summing right-nested trees", rightNestedTrees)

  val concatJaggedNestedBench =
    ConcatAndSumTreeBenchmark("concatenating and summing jagged trees", jaggedNestedTrees)

  val concatBalancedNestedBench =
    ConcatAndSumTreeBenchmark("concatenating and summing balanced trees", balancedNestedTrees)

  val sequenceVarieties: Seq[Variety[Sequence]] = Seq(
    Variety[Sequence, List]("list"),
    Variety[Sequence, Vector]("vec"),
    Variety[Sequence, Queue]("que"),
    Variety[Sequence, Catenable]("cat"),
    Variety[Sequence, CatenableArrLeaves]("catarr"),
    Variety[Sequence, TurtleQ]("turtle")
  )

  val allSeqBenchmarks: Seq[Benchmark[Sequence]] = Seq(
    sumConsBench, sumSnocBench,
    queueBench, stackBench,
    consConcatBenchLeft, consConcatBenchRight,
    snocConcatBenchLeft, snocConcatBenchRight,
    concatLeftNestedBench, concatRightNestedBench,
    concatJaggedNestedBench,
    concatBalancedNestedBench
  )

  val seqBenchSuite: BenchmarkSuite[Sequence] =
    BenchmarkSuite[Sequence](sequenceVarieties, allSeqBenchmarks)
}
