package scabs.free.ap

import scabs.Util.{Applicative, ~>}
import scabs.seq.Sequence
import shapeless._

trait ApSequenceDirect[S[_], F[_], A] {
  type U <: HList

  def ua: U => A

  def apSequence: ApSequence[F, F, U]
}

object ApSequenceDirect {
  def lift[S[_], F[_], A](fa: F[A])(implicit S: Sequence[S]): ApSequenceDirect[S, F, A] = new ApSequenceDirect[S, F, A] {
    override type U = A :: HNil
    override def ua: U => A = {
      case a :: _ => a
    }
    override def apSequence: ApSequence[S, F, A :: HNil] = ApSequence.empty[S, F] :+ fa
  }

  def retract[S[_], F[_], A](fa: ApSequenceDirect[S, F, A])(implicit F: Applicative[F]): F[A] = {
    F.fmap(fa.apSequence.retract)(fa.ua)
  }

  def foldMap[S[_], F[_], G[_], A](fa: ApSequenceDirect[S, F, A], trans: F ~> G)(implicit G: Applicative[G]): G[A] = {
    G.fmap(fa.apSequence.foldMap(trans))(fa.ua)
  }

}
