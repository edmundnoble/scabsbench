package scabs

import org.scalacheck.rng.Seed
import org.scalatest.FreeSpec

abstract class ScalatestSuiteK[Typeclass[_[_]]](suite: TestSuiteK[Typeclass]) extends FreeSpec {

  import org.scalacheck._

  suite.varieties.foreach { variety =>
    implicit val instance = variety.instance
    variety.name - {
      var c = Seed(3215471L)
      suite.tests.foreach { test =>
        test.name in {
          c = c.next
          val input = test.generateInput.apply(Gen.Parameters.default, c)
          test.runTest(input.get)
        }
      }
    }
  }
}
