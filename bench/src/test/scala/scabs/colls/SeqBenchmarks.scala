package scabs.colls

import java.nio.ByteBuffer

import org.scalameter.api._
import org.scalameter.picklers.{IntPickler, PrimitivePickler}
import scabs.colls.Hierarchy._
import scabs.colls.StdlibInstances._

import scala.collection.immutable.Queue

object SeqBenchmarks extends java.io.Serializable {

  implicit def intPickler = IntPickler

  implicit def twoIntPickler = new PrimitivePickler[(Int, Int)] {
    override protected def bits: Int = java.lang.Integer.SIZE * 2
    override protected def unwrap(from: ByteBuffer): (Int, Int) = (from.getInt(), from.getInt())
    override def pickle(x: (Int, Int)): Array[Byte] = byteBuffer.putInt(x._1).putInt(x._2).array()
  }

  def cSequenceVarieties = Seq(
    BenchVariety(CSequence[List], "list").forget,
    BenchVariety(CSequence[Vector], "vec").forget,
    BenchVariety(CSequence[OkasakiQueue], "que").forget,
    BenchVariety(CSequence[HQueue], "hque").forget,
    BenchVariety(CSequence[Catenable], "cat").forget,
    BenchVariety(CSequence[Queue], "sque").forget,
    BenchVariety(CSequence[Stream], "stream").forget
  )

  def sequenceVarieties = Seq(
    BenchVariety(Sequence[List], "list").forget,
    BenchVariety(Sequence[Vector], "vec").forget,
    BenchVariety(Sequence[OkasakiQueue], "que").forget,
    BenchVariety(Sequence[HQueue], "hque").forget,
    BenchVariety(Sequence[Catenable], "cat").forget,
    BenchVariety(Sequence[Queue], "sque").forget,
    BenchVariety(Sequence[Stream], "stream").forget
  )

  import SeqManipulators._

  type Const[A, B] = A
  type Id[A] = A
  type TupleRight[A, B] = (B, A)

  def consRecBench = new ConstBenchmark[Sequence, Int]("cons", constructSizes) {
    override def run[F[_] : Sequence](i: Int): Any = consRec[F](i)
  }

  def snocRecBench = new ConstBenchmark[Sequence, Int]("snoc", constructSizes) {
    override def run[F[_] : Sequence](i: Int): Any = snocRec[F](i)
  }

  def sumConsBench = new Benchmark[Sequence, Id, Int] {
    override def name = "summing cons-constructed seq"
    override def run[F[_] : Sequence](i: F[Int]): Any = sum[F](i)
    override def gen[F[_] : Sequence]: Gen[F[Int]] = testConsSeqOnes[F]
  }

  def sumSnocBench = new Benchmark[Sequence, Id, Int] {
    override def name: String = "summing snoc-constructed seq"
    override def run[F[_] : Sequence](i: F[Int]): Any = sum[F](i)
    override def gen[F[_] : Sequence]: Gen[F[Int]] = testSnocSeqOnes[F]
  }

  def queueBench = new Benchmark[Sequence, TupleRight[Int, ?], Int] {
    override def name: String = "queueing (alternating snoc and tail)"
    override def run[F[_] : Sequence](i: (F[Int], Int)): Any = queue[F](i._1, i._2)
    override def gen[F[_] : Sequence]: Gen[(F[Int], Int)] = testSnocSeqOnes[F] zip queueBenchSizes
  }

  def consConcatBenchRight = new Benchmark[CSequence, List, Int] {
    override def name: String = "concatenating and summing cons-constructed seqs to the right"
    override def run[F[_] : CSequence](i: List[F[Int]]): Any = concatRight[F](i)
    override def gen[F[_] : CSequence]: Gen[List[F[Int]]] = consStructsToConcat[F]
  }

  def snocConcatBenchRight = new Benchmark[CSequence, List, Int] {
    override def name: String = "concatenating and summing snoc-constructed seqs to the right"
    override def run[F[_] : CSequence](i: List[F[Int]]): Any = concatRight[F](i)
    override def gen[F[_] : CSequence]: Gen[List[F[Int]]] = snocStructsToConcat[F]
  }

  def consConcatBenchLeft = new Benchmark[CSequence, List, Int] {
    override def name: String = "concatenating and summing cons-constructed seqs to the left"
    override def run[F[_] : CSequence](i: List[F[Int]]): Any = concatLeft[F](i)
    override def gen[F[_] : CSequence]: Gen[List[F[Int]]] = consStructsToConcat[F]
  }

  def snocConcatBenchLeft = new Benchmark[CSequence, List, Int] {
    override def name: String = "concatenating and summing snoc-constructed seqs to the left"
    override def run[F[_] : CSequence](i: List[F[Int]]): Any = concatLeft[F](i)
    override def gen[F[_] : CSequence]: Gen[List[F[Int]]] = snocStructsToConcat[F]
  }

  def concatLeftNestedBench =
    new ConstBenchmark[CSequence, LTree[Int]]("concatenating left-nested trees", leftNestedTrees) {
      override def run[F[_] : CSequence](i: LTree[Int]): Any = frontierRec[F, Int](i)
    }

  def concatRightNestedBench =
    new ConstBenchmark[CSequence, LTree[Int]]("concatenating right-nested trees", rightNestedTrees) {
      override def run[F[_] : CSequence](i: LTree[Int]): Any = frontierRec[F, Int](i)
    }

  def concatJaggedNestedBench =
    new ConstBenchmark[CSequence, LTree[Int]]("concatenating jagged trees", jaggedNestedTrees) {
      override def run[F[_] : CSequence](i: LTree[Int]): Any = frontierRec[F, Int](i)
    }

  def concatBalancedNestedBench =
    new ConstBenchmark[CSequence, LTree[Int]]("concatenating balanced trees", balancedNestedTrees) {
      override def run[F[_] : CSequence](i: LTree[Int]): Any = frontierRec[F, Int](i)
    }

  def funConcatLeftNestedBench =
    new ConstBenchmark[Sequence, LTree[Int]]("functionally concatenating left-nested trees", leftNestedTrees) {
      override def run[F[_] : Sequence](i: LTree[Int]): Any = frontierCPS[F, Int](i)
    }

  def funConcatRightNestedBench =
    new ConstBenchmark[Sequence, LTree[Int]]("functionally concatenating right-nested trees", rightNestedTrees) {
      override def run[F[_] : Sequence](i: LTree[Int]): Any = frontierCPS[F, Int](i)
    }

  def funConcatJaggedNestedBench =
    new ConstBenchmark[Sequence, LTree[Int]]("functionally concatenating jagged trees", jaggedNestedTrees) {
      override def run[F[_] : Sequence](i: LTree[Int]): Any = frontierCPS[F, Int](i)
    }

  def funConcatBalancedNestedBench =
    new ConstBenchmark[Sequence, LTree[Int]]("functionally concatenating balanced trees", balancedNestedTrees) {
      override def run[F[_] : Sequence](i: LTree[Int]): Any = frontierCPS[F, Int](i)
    }

  def allSeqBenchmarks: Seq[Benchmark[Sequence, Nothing, Nothing]] = Seq(
    consRecBench.forget, snocRecBench.forget,
    sumConsBench.forget, sumSnocBench.forget,
    queueBench.forget,
    funConcatLeftNestedBench.forget, funConcatRightNestedBench.forget,
    funConcatJaggedNestedBench.forget,
    funConcatBalancedNestedBench.forget
  )

  def allCSeqBenchmarks: Seq[Benchmark[CSequence, Nothing, Nothing]] = Seq(
    consConcatBenchLeft.forget, consConcatBenchRight.forget,
    snocConcatBenchLeft.forget, snocConcatBenchRight.forget,
    concatLeftNestedBench.forget, concatRightNestedBench.forget,
    concatJaggedNestedBench.forget,
    concatBalancedNestedBench.forget
  )

  val seqBenchSuite: BenchSuite[Sequence] = new BenchSuite[Sequence] {
    override def varieties: Seq[BenchVariety[Sequence, Nothing]] = sequenceVarieties
    override def benchmarks: Seq[Benchmark[Sequence, Nothing, Nothing]] = allSeqBenchmarks
  }

  val cSeqBenchSuite: BenchSuite[CSequence] = new BenchSuite[CSequence] {
    override def varieties: Seq[BenchVariety[CSequence, Nothing]] = cSequenceVarieties
    override def benchmarks: Seq[Benchmark[CSequence, Nothing, Nothing]] = allCSeqBenchmarks
  }

  val benchSuites = Seq(seqBenchSuite.forget, cSeqBenchSuite.forget)
}
