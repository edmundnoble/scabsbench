package scabs

import org.scalameter.api.Gen

abstract class Benchmark[Typeclass[_[_]]](val name: String) extends Serializable {
  type InputWrapper[_]
  type Input

  def generateInput[F[_] : Typeclass]: Gen[InputWrapper[F[Input]]]

  def runBenchmark[F[_] : Typeclass](input: InputWrapper[F[Input]]): Any
}

object Benchmark {

  abstract class AuxC[Typeclass[_[_]], InputWrapper0[_], Input0](name0: String) extends Benchmark[Typeclass](name0) {
    type InputWrapper[A] = InputWrapper0[A]
    type Input = Input0
  }

  type Aux[Typeclass[_[_]], InputWrapper0[_], Input0] = Benchmark[Typeclass] {
    type InputWrapper[A] = InputWrapper0[A]
    type Input = Input0
  }

}
