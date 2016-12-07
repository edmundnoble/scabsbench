package scabs.free

import scabs.Util.{Monad, ~>}

case class FreeApEither[F[_], A](run: Either[F[FreeApEither[F, A]], A]) extends AnyVal
sealed trait FreeApADT[F[_], A] {
  def foldMap[G[_] : Ap](trans: F ~> G): G[A]
}
object FreeApADT {
  case class PureApADT[F[_], A](value: A) extends FreeApADT[F, A] {
    override def foldMap[G[_] : Ap](trans: ~>[F, G]): G[A] = Ap[G].pure(value)
  }
  case class RollApADT[F[_], A](roll: F[FreeApADT[F, A]]) extends FreeApADT[F, A] {
    override def foldMap[G[_] : Ap](trans: ~>[F, G]): G[A] = Ap[G].bind(trans(roll))(_.foldMap[G](trans))
  }
}
