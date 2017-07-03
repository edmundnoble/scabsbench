package scabs

import org.scalacheck.Gen
import org.scalatest.prop.Configuration

abstract class ConstantInputTest[C[_[_]], In](testName: String, override val generateInput: Gen[In],
                                              testParameters: Seq[Configuration#PropertyCheckConfigParam] = Seq.empty) extends Test[C](testName) {
  type Input = In
}
