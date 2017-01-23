package scabs

import org.scalameter.Bench

case class TCBenchmarkReport[S[_[_]]](suite: TCBenchSuite[S]) extends Bench.OfflineReport with Serializable {
  TCScalameterRunner.runSuite[S](this, suite)
}
