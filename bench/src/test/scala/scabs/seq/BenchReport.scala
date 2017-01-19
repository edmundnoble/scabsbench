package scabs.seq

import scabs.TCBenchmarkReport

object BenchReport extends TCBenchmarkReport[Sequence](Benchmarks.seqBenchSuite)
