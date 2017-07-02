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
  def varieties: Seq[Variety[Sequence]] = {
    Seq(
      Variety[Sequence, List]("list"),
      Variety[Sequence, Vector]("vec"),
      Variety[Sequence, Queue]("que"),
      Variety[Sequence, scabs.seq.Catenable]("cat"),
      Variety[Sequence, scabs.seq.CatenableArrLeaves]("catarr"),
      Variety[Sequence, scabs.seq.TurtleQ]("turtle"),
      Variety[Sequence, scabs.seq.InceptionQ]("inceptionQ")
    )
  }

  abstract class StackOpsTest(name0: String) extends Test.ImplicitInput[Sequence, StackOps[Int]](name0)

  def consUnconsIdentity = {
    new StackOpsTest("uncons(cons(s, x)) == (s, x)") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Assertion = {
        val input = ops.replay[F]
        equal(
          Sequence[F].uncons(Sequence[F].cons(1, input)),
          Some((1, input))
        )
      }
    }
  }

  def snocUnsnocIdentity = {
    new StackOpsTest("unsnoc(snoc(x, s)) == (x, s)") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Assertion = {
        val input = ops.replay[F]
        equal(
          F.uncons(F.snoc(input, "test")),
          Some(("test", input))
        )
      }
    }
  }

  def consIncrementsLength = {
    new StackOpsTest("cons(x, s).length = s.length + 1") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Assertion = {
        val input = ops.replay[F]
        equal(
          F.lengthSeq(F.cons("test", input)),
          F.lengthSeq(input) + 1
        )
      }
    }
  }

  def snocIncrementsLength = {
    new StackOpsTest("snoc(s, x).length = s.length + 1") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Assertion = {
        val input = ops.replay[F]
        equal(
          F.lengthSeq(F.snoc(input, "test")),
          F.lengthSeq(input) + 1
        )
      }
    }
  }

  def unconsDecrementsLength = new StackOpsTest("") {
    def runTest[F[_]](input: StackOps[Int])(implicit F: Sequence[F]): Assertion = {

    }
  }


  def tests: Seq[Test[Sequence]] = {
    Seq(
      consUnconsIdentity, snocUnsnocIdentity, arbStackOps
    )
  }

  val suite =
    TestSuiteK(varieties, tests)
}
