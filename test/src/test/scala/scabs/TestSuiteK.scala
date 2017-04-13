package scabs

abstract class TestSuiteK[C[_[_]]] {
  val varieties: Seq[Variety[C]]
  val tests: Seq[Test[C]]
}

object TestSuiteK {
  def apply[C[_[_]]](varieties0: Seq[Variety[C]], tests0: Seq[Test[C]]): TestSuiteK[C] =
    new TestSuiteK[C] {
      val varieties: Seq[Variety[C]] = varieties0
      val tests: Seq[Test[C]] = tests0
    }
}

trait TestRunner[Ctx] {
  def runSuite[C[_[_]]](ctx: Ctx, suite: TestSuiteK[C]): Unit
}
