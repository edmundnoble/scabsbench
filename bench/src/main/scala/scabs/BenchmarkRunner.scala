package scabs

trait BenchmarkRunner[Ctx] {
  def runSuite[C[_[_]]](ctx: Ctx, suite: BenchmarkSuite[C]): Unit
}
