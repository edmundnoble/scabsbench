package scabs

import org.scalacheck.{Gen, Test => SCTest}
import org.scalatest.prop.Configuration

abstract class Test[Typeclass[_[_]]](val name: String,
                                     val testParameters: Seq[Configuration#PropertyCheckConfigParam] = Seq.empty)
  extends Assertion.DSL with Serializable {
  type Input
  val generateInput: Gen[Input]

  def runTest[F[_] : Typeclass](input: Input): Assertion
}

object Test {

  abstract class AuxC[Typeclass[_[_]], Input0](name0: String,
                                               testParameters: Seq[Configuration#PropertyCheckConfigParam] = Seq.empty)
    extends Test[Typeclass](name0, testParameters) {
    type Input = Input0
  }

  abstract class ImplicitInput[Typeclass[_[_]], Input0](name0: String,
                                                        testParameters: Seq[Configuration#PropertyCheckConfigParam] = Seq.empty)(
                                                         implicit override val generateInput: Gen[Input0])
    extends Test[Typeclass](name0, testParameters) {
    type Input = Input0
  }

  type Aux[Typeclass[_[_]], Input0] = Test[Typeclass] {
    type Input = Input0
  }

}

