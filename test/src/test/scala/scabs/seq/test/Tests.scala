package scabs
package seq
package test

import org.scalacheck.Gen
import StdlibInstances._
import scabs.Util._

import scala.collection.immutable.Queue

final class Tests extends ScabsScalatestSuiteK[Sequence] {
  def varieties: Seq[Variety[Sequence]] = Seq(
    Variety[Sequence, List]("list"),
    Variety[Sequence, Vector]("vec"),
    Variety[Sequence, Queue]("que"),
    Variety[Sequence, Catenable]("cat"),
    Variety[Sequence, CatenableArrLeaves]("catarr"),
    Variety[Sequence, TurtleQ]("turtle")
  )

  abstract class StringInputTest(name0: String) extends Test.AuxC[Sequence, Id, String](name0) {
    def generateInput[F[_] : Sequence]: Gen[String] =
      Gen.alphaNumStr
  }

  def consUncons = new StringInputTest("uncons(cons(s, x)) == (s, x)") {
    def runTest[F[_] : Sequence](input: String): Assertion =
      equal(
        Sequence[F].uncons(Sequence[F].cons(input, Sequence[F].empty[String])),
        Some((input, Sequence[F].empty[String]))
      )
  }

  def snocUnsnoc = new StringInputTest("unsnoc(snoc(x, s)) == (x, s)") {
    def runTest[F[_] : Sequence](input: String): Assertion =
      equal(
        Sequence[F].uncons(Sequence[F].snoc(Sequence[F].empty[String], input)),
        Some((input, Sequence[F].empty[String]))
      )
  }

  def tests: Seq[Test[Sequence]] = Seq(
    consUncons, snocUnsnoc
  )
}
