package scabs

import org.scalacheck.Gen

abstract class Test[Typeclass[_[_]]](val name: String) extends Assertion.DSL with Serializable {
  type InputWrapper[_]
  type Input

  def generateInput[F[_] : Typeclass]: Gen[InputWrapper[Input]]

  def runTest[F[_] : Typeclass](input: InputWrapper[Input]): Assertion
}

object Test {

  abstract class AuxC[Typeclass[_[_]], InputWrapper0[_], Input0](name0: String)
    extends Test[Typeclass](name0) {
    type InputWrapper[A] = InputWrapper0[A]
    type Input = Input0
  }

  type Aux[Typeclass[_[_]], InputWrapper0[_], Input0] = Test[Typeclass] {
    type InputWrapper[A] = InputWrapper0[A]
    type Input = Input0
  }

}

