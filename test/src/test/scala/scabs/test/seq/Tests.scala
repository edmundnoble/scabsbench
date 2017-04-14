package scabs
package test
package seq

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.prop.Checkers
import scabs.Util._
import cats._
import cats.implicits._
import scabs.seq.Sequence
import scabs.seq.StdlibInstances._

import scala.collection.immutable.Queue

object Tests {
  def varieties: Seq[Variety[Sequence]] = Seq(
    Variety[Sequence, List]("list"),
    Variety[Sequence, Vector]("vec"),
    Variety[Sequence, Queue]("que"),
    Variety[Sequence, scabs.seq.Catenable]("cat"),
    Variety[Sequence, scabs.seq.CatenableArrLeaves]("catarr"),
    Variety[Sequence, scabs.seq.TurtleQ]("turtle"),
    Variety[Sequence, scabs.seq.InceptionQ]("inceptionQ")
  )

  abstract class StringInputTest(name0: String) extends Test.AuxC[Sequence, Id, String](name0) {
    def generateInput[F[_] : Sequence]: Gen[String] =
      Gen.alphaNumStr
  }

  abstract class StackOpsTest(name0: String) extends Test.AuxC[Sequence, Id, StackOps[Int]](name0) {
    override def generateInput[F[_] : Sequence]: Gen[StackOps[Int]] = {
      StackOps.genOps
    }
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

  def arbStackOps = new StackOpsTest("stack size") {
    override def runTest[F[_] : Sequence](input: StackOps[Int]): Assertion = {
      val S = implicitly[Sequence[F]]

      val asList = StackOps.taglessFinal.replay[List, Int](input)
      val asSeq = StackOps.taglessFinal.replay[F, Int](input)

      println(input + "\n" + asList + "\n" + asSeq)

      equal(asList.size,
        S.lengthSeq(asSeq))
    }
  }

  def tests: Seq[Test[Sequence]] = Seq(
    consUncons, snocUnsnoc, arbStackOps
  )

  val suite =
    TestSuiteK(varieties, tests)
}
