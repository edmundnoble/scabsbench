package scabs
package bench
package seq

import scabs.seq.Sequence

object BenchReport extends BenchmarkReport[Sequence](Benchmarks.seqBenchSuite)
