package scabs.colls

import org.scalameter.api._

object SeqBench
  extends Bench.OfflineReport with java.io.Serializable {

  // actually run everything (important step)
  SeqBenchmarks.benchSuites.foreach(ScalameterRunner.runSuite[Nothing](this, _))

}
