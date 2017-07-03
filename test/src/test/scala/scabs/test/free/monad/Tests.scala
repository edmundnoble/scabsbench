package scabs
package test
package free
package monad

import cats.Monad
import org.scalacheck.{Gen, Prop}
import scabs.Util._
import scabs._
import scabs.free.Constraint.FreeMonad
import scabs.seq.{Catenable, TurtleQ}
import AssertionT._

final class Tests[F[_]: Monad] {
  type FM[V[_]] = FreeMonad[F, V]
  def varieties: Seq[Variety[FM]] = Seq(
     Variety[FM, scabs.free.monad.ADTCPS[F, ?]]("adtcps"),
     Variety[FM, scabs.free.monad.RWR[Catenable, F, ?]]("rwrcat"),
     Variety[FM, scabs.free.monad.RWR[TurtleQ, F, ?]]("rwrturt"),
     Variety[FM, scabs.free.monad.RWRPlusMap[Catenable, F, ?]]("rwrmapcat"),
     Variety[FM, scabs.free.monad.RWRPlusMap[TurtleQ, F, ?]]("rwrmapturt"),
     Variety[FM, scabs.free.monad.Tagless[F, ?]]("tagless")
  )

  def identityTest[A](input: Gen[F[A]]): ConstantInputTest[FM, F[A]] =
    new ConstantInputTest[FM, F[A]]("identity", input) {
      def runTest[R[_] : FM](input: F[A]): Prop = {
        val fm = implicitly[FM[R]]
        Equal(input, fm.retract(fm.generated.flatMap(fm.lift(input))(fm.generated.pure))).toProp
      }
    }

  def tests: Seq[Test[FM]] = Seq(
    identityTest[Int](Gen.const(Monad[F].pure(1)))
  )

  val suite = TestSuiteK[FM](varieties, tests)
}

