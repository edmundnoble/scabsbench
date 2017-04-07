package scabs

abstract class BenchmarkSuite[C[_[_]]] {
  val varieties: Seq[Variety[C]]
  val benchmarks: Seq[Benchmark[C]]
}

object BenchmarkSuite {
  def apply[C[_[_]]](varieties0: Seq[Variety[C]], benchmarks0: Seq[Benchmark[C]]): BenchmarkSuite[C] =
    new BenchmarkSuite[C] {
      val varieties: Seq[Variety[C]] = varieties0
      val benchmarks: Seq[Benchmark[C]] = benchmarks0
    }
}

