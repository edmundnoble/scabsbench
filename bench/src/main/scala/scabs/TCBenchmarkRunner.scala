package scabs

trait TCBenchmarkRunner[Ctx] {
  def runSuite[C[_[_]]](ctx: Ctx, suite: TCBenchSuite[C]): Unit
}
