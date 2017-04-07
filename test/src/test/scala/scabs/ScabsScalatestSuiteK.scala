package scabs

import org.scalacheck.rng.Seed
import org.scalatest.FreeSpec

abstract class ScabsScalatestSuiteK[Typeclass[_[_]]] extends FreeSpec {

  def varieties: Seq[Variety[Typeclass]]
  def tests: Seq[Test[Typeclass]]

  import org.scalacheck._

  varieties.foreach { variety =>
    implicit val instance = variety.instance
    s"variety ${variety.name}" - {
      var c = Seed(3215471L)
      tests.foreach { test =>
        s"test ${test.name}" in {
          c = c.next
          val input = test.generateInput.apply(Gen.Parameters.default, c)
          test.runTest(input.get)
        }
      }
    }
  }

}
