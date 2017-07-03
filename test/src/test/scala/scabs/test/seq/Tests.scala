package scabs
package test
package seq

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.prop.Checkers
import AssertionT._
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
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Prop = {
        val input = ops.replay[F]
        Equal(
          Sequence[F].uncons(Sequence[F].cons(1, input)),
          Some((1, input))
        ).toProp
      }
    }
  }

  def snocUnsnocIdentity = {
    new StackOpsTest("unsnoc(snoc(x, s)) == (x, s)") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Prop = {
        val input = ops.replay[F]
        Equal(
          F.uncons(F.snoc(input, 1)),
          Some(("test", input))
        ).toProp
      }
    }
  }

  def consIncrementsLength = {
    new StackOpsTest("cons(x, s).length = s.length + 1") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Prop = {
        val input = ops.replay[F]
        Equal(
          F.lengthSeq(F.cons(1, input)),
          F.lengthSeq(input) + 1
        ).toProp
      }
    }
  }

  def snocIncrementsLength = {
    new StackOpsTest("snoc(s, x).length = s.length + 1") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Prop = {
        val input = ops.replay[F]
        Equal(
          F.lengthSeq(F.snoc(input, 1)),
          F.lengthSeq(input) + 1
        ).toProp
      }
    }
  }

  def unconsDecrementsLength = {
    new StackOpsTest("unsnoc(s).length") {
      def runTest[F[_]](ops: StackOps[Int])(implicit F: Sequence[F]): Prop = {
        val input = ops.replay[F]
        Equal(
          F.uncons(input).map(f => F.lengthSeq(f._2)).getOrElse(0),
          if (F.isEmpty(input)) 0 else F.lengthSeq(input) - 1
        ).toProp
      }
    }
  }


  def tests: Seq[Test[Sequence]] = {
    Seq(
      consUnconsIdentity, snocUnsnocIdentity, consIncrementsLength, snocIncrementsLength
    )
  }

  val suite =
    TestSuiteK(varieties, tests)
}
