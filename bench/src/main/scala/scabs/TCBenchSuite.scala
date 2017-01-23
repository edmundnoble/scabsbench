package scabs

case class TCBenchSuite[C[_[_]]](varieties: Seq[TCBenchVariety[C, Nothing]],
                                 benchmarks: Seq[TCBenchmark[C, Nothing, Nothing]]) {
  final def forget: TCBenchSuite[Nothing] = this.asInstanceOf[TCBenchSuite[Nothing]]
}

