package scabs

import org.scalameter.api.Gen

abstract class TCBenchmark[C[_[_]], M[_], I] extends Serializable {
  def name: String
  def gen[F[_] : C]: Gen[M[F[I]]]
  def run[F[_] : C]: M[F[I]] => Any
  final def forget: TCBenchmark[C, Nothing, Nothing] =
    this.asInstanceOf[TCBenchmark[C, Nothing, Nothing]]
}
