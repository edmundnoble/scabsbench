package scabs

import org.scalacheck.{Gen, Test => SCTest}
import org.scalatest.prop.Configuration

abstract class Test[Typeclass[_[_]]](val name: String,
                                     val testParameters: Seq[Configuration#PropertyCheckConfigParam] = Seq.empty)
  extends Assertion.DSL with Serializable {
  type InputWrapper[_]
  type Input

  def generateInput[F[_] : Typeclass]: Gen[InputWrapper[Input]]

  def runTest[F[_] : Typeclass](input: InputWrapper[Input]): Assertion
}

object Test {

  abstract class AuxC[Typeclass[_[_]], InputWrapper0[_], Input0](name0: String,
                                                                 testParameters: Seq[Configuration#PropertyCheckConfigParam] = Seq.empty)
    extends Test[Typeclass](name0, testParameters) {
    type InputWrapper[A] = InputWrapper0[A]
    type Input = Input0
  }

  type Aux[Typeclass[_[_]], InputWrapper0[_], Input0] = Test[Typeclass] {
    type InputWrapper[A] = InputWrapper0[A]
    type Input = Input0
  }

}

