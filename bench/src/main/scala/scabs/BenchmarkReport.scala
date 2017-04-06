package scabs

import org.scalameter.Bench

class BenchmarkReport[C[_[_]]](suite: BenchmarkSuite[C]) extends Bench.OfflineReport with Serializable {
  ScalameterRunner.runSuite(this, suite)
}
