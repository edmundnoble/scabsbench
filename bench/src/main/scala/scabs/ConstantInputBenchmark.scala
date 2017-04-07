package scabs

import org.scalameter.api.Gen
import scabs.Util.Const

abstract class ConstantInputBenchmark[C[_[_]], In](benchName: String, input: Gen[In]) extends Benchmark[C](benchName) {
  type InputWrapper[A] = In
  type Input = In
  override def generateInput[F[_] : C]: Gen[In] = input
}

