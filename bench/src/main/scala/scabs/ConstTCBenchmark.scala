package scabs

import org.scalameter.api.Gen
import scabs.Util.Const

abstract class ConstTCBenchmark[C[_[_]], In](benchName: String, input: Gen[In])
  extends TCBenchmark[C, Const[In, ?], Nothing] {
  override def name: String = benchName
  override def gen[F[_] : C]: Gen[In] = input
}

