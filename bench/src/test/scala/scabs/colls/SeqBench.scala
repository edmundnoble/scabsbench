package scabs.colls

import org.scalameter.api._
import scabs.TCScalameterRunner

object SeqBench
  extends Bench.OfflineReport with java.io.Serializable {

  // actually run everything (important step)
  SeqBenchmarks.benchSuites.foreach(TCScalameterRunner.runSuite[Nothing](this, _))

}
