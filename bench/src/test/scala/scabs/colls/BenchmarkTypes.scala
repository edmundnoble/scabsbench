package scabs.colls

import org.scalameter.api._
import scabs.Util.Const

final case class BenchVariety[S[_[_]], M[_]](
                                              instance: S[M],
                                              name: String
                                            ) {
  def forget: BenchVariety[S, Nothing] =
    this.asInstanceOf[BenchVariety[S, Nothing]]
}

abstract class Benchmark[S[_[_]], M[_], I] extends Serializable {
  def name: String
  def gen[F[_] : S]: Gen[M[F[I]]]
  def run[F[_] : S](i: M[F[I]]): Any
  final def forget: Benchmark[S, Nothing, Nothing] =
    this.asInstanceOf[Benchmark[S, Nothing, Nothing]]
}

abstract class ConstBenchmark[S[_[_]], In](benchName: String, input: Gen[In])
  extends Benchmark[S, Const[In, ?], Nothing] {
  override def name: String = benchName
  override def gen[F[_] : S]: Gen[In] = input
}

abstract class BenchSuite[S[_[_]]] {
  def varieties: Seq[BenchVariety[S, Nothing]]
  def benchmarks: Seq[Benchmark[S, Nothing, Nothing]]
  final def forget: BenchSuite[Nothing] = this.asInstanceOf[BenchSuite[Nothing]]
}

trait BenchmarkRunner[Ctx] {
  def runSuite[S[_[_]]](ctx: Ctx, suite: BenchSuite[S]): Unit
}

object ScalameterRunner extends BenchmarkRunner[Bench[Double]] {

  private def runWithVariety[S[_[_]], M[_], R[_], I](bench: Bench[Double], benchmark: Benchmark[S, R, I], variety: BenchVariety[S, M]): Unit = {
    implicit val instance = variety.instance
    bench.measure method variety.name in {
      bench.using(benchmark.gen[M]) in benchmark.run[M]
    }
  }

  override def runSuite[S[_[_]]](ctx: Bench[Double], suite: BenchSuite[S]): Unit = {
    suite.benchmarks.foreach { b =>
      ctx.performance of b.name in {
        suite.varieties.foreach(runWithVariety[S, Nothing, Nothing, Nothing](ctx, b, _))
      }
    }
  }

}
