package scabs

import org.scalacheck.rng.Seed
import org.scalatest.FreeSpec
import org.scalatest.prop.{Checkers, PropertyChecks}

abstract class ScalatestSuiteK[Typeclass[_[_]]](suite: TestSuiteK[Typeclass]) extends FreeSpec with PropertyChecks {

  import org.scalacheck._

  suite.varieties.foreach(variety => {
    implicit val instance: Typeclass[variety.Type] = variety.instance
    variety.name - {
      suite.tests.foreach(test => {
        test.name in {
          forAll(
            test.generateInput,
            test.testParameters.asInstanceOf[Seq[PropertyCheckConfigParam]] : _*
          ) { i =>
            test.runTest[variety.Type](i) : Prop
          }
        }
      })
    }
  })
}
