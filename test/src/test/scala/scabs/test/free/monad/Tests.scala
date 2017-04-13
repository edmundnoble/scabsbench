package scabs
package test
package free
package monad

import org.scalacheck.Gen
import scabs.Util._
import scabs._
import scabs.free.Constraint.FreeMonad
import scabs.free.monad.{ADTCPS, RWR, RWRPlusMap, Tagless}
import scabs.seq.{Catenable, TurtleQ}

final class Tests[F[_]: Monad] {
  type FM[V[_]] = FreeMonad[F, V]
  def varieties: Seq[Variety[FM]] = Seq(
    // Variety[FM, ADTCPS[F, ?]]("adtcps"),
    // Variety[FM, RWR[Catenable, F, ?]]("rwrcat"),
    // Variety[FM, RWR[TurtleQ, F, ?]]("rwrturt"),
    // Variety[FM, RWRPlusMap[Catenable, F, ?]]("rwrmapcat"),
    // Variety[FM, RWRPlusMap[TurtleQ, F, ?]]("rwrmapturt"),
    // Variety[FM, Tagless[F, ?]]("tagless")
  )

  def identityTest[A](input: Gen[F[A]]): ConstantInputTest[FM, F[A]] =
    new ConstantInputTest[FM, F[A]]("identity", input) {
      def runTest[R[_] : FM](input: F[A]): Assertion = {
        val fm = implicitly[FM[R]]
        equal(input, fm.retract(fm.generated.bind(fm.lift(input))(fm.generated.pure)))
      }
    }

  def tests: Seq[Test[FM]] = Seq(
    identityTest[Int](Gen.const(Monad[F].pure(1)))
  )

  val suite = TestSuiteK[FM](varieties, tests)
}

