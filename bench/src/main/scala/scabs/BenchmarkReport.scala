package scabs

import org.scalameter.Bench

final class BenchmarkReport[C[_[_]]](suite: BenchmarkSuite[C]) extends Bench.OfflineReport with Serializable {
  ScalameterRunner.runSuite(this, suite)
}
