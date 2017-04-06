package scabs

import org.scalameter.api.Bench

object ScalameterRunner extends BenchmarkRunner[Bench[Double]] {

  private def runWithVariety[C[_[_]], M[_], R[_], I](bench: Bench[Double], benchmark: Benchmark.Aux[C, R, I], variety: Variety.Aux[C, M]): Unit = {
    implicit val instance: C[M] = variety.instance
    bench.measure method variety.name in {
      bench.using(benchmark.generateInput[M]) in benchmark.runBenchmark[M]
    }
  }

  def runSuite[C[_[_]]](ctx: Bench[Double], suite: BenchmarkSuite[C]): Unit = {
    suite.benchmarks.foreach { benchmark =>
      ctx.performance of benchmark.name in {
        suite.varieties.foreach(variety => runWithVariety[C, variety.Type, benchmark.InputWrapper, benchmark.Input](ctx, benchmark, variety))
      }
    }
  }

}
