package scabs

import org.scalacheck.Gen

abstract class ConstantInputTest[C[_[_]], In](testName: String, input: Gen[In]) extends Test[C](testName) {
  type InputWrapper[A] = In
  type Input = In
  override def generateInput[F[_] : C]: Gen[In] = input
}
