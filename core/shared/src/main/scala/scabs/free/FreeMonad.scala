package scabs.free

import scabs.Util._
import scabs.colls.CSequence

import scala.annotation.tailrec

case class FreeMonadEither[F[_], A](run: Either[F[FreeMonadEither[F, A]], A]) extends AnyVal
sealed trait FreeMonadADT[F[_], A] {
  def foldMap[G[_] : MonadBind](trans: F ~> G): G[A]
}
object FreeMonadADT {
  case class PureMonadADT[F[_], A](value: A) extends FreeMonadADT[F, A] {
    override def foldMap[G[_] : MonadBind](trans: ~>[F, G]): G[A] = MonadBind[G].pure(value)
  }
  case class RollMonadADT[F[_], A](roll: F[FreeMonadADT[F, A]]) extends FreeMonadADT[F, A] {
    override def foldMap[G[_] : MonadBind](trans: ~>[F, G]): G[A] = MonadBind[G].bind(trans(roll))(_.foldMap[G](trans))
  }
}

sealed trait FreeMonadADTCPS[F[_], A] {
  def foldMap[G[_] : MonadBind](trans: F ~> G): G[A]
}
object FreeMonadADTCPS {
  case class Pure[F[_], A](value: A) extends FreeMonadADTCPS[F, A] {
    override def foldMap[G[_] : MonadBind](trans: ~>[F, G]): G[A] = MonadBind[G].pure(value)
  }
  case class Roll[F[_], A](roll: F[A]) extends FreeMonadADTCPS[F, A] {
    override def foldMap[G[_] : MonadBind](trans: ~>[F, G]): G[A] = trans(roll)
  }
  case class Bind[F[_], A, B](free: FreeMonadADTCPS[F, A], bind: A => F[B]) extends FreeMonadADTCPS[F, B] {
    override def foldMap[G[_] : MonadBind](trans: ~>[F, G]): G[B] =
      MonadBind[G].bind(free.foldMap(trans))(a => trans(bind(a)))
  }
}

sealed trait FreeMonadReflect[S[_], F[_], A] {
  @tailrec
  final def run(v: Any, nexts: S[Any => FreeMonadReflect[S, F, Any]])(implicit S: CSequence[S]): FreeMonadReflect[S, F, Any] = {
    if (S.sequence.isEmpty(nexts)) {
      FreeMonadReflect.Pure(v)
    } else {
      val Some((head, tail)) = S.sequence.uncons(nexts)
      head(v) match {
        case FreeMonadReflect.Pure(a) => run(a, tail)
        case FreeMonadReflect.Seq(nv, s) => FreeMonadReflect.Seq[S, F, Any](nv, S.concat(s, tail))
      }
    }
  }

  def foldMap[G[_]](trans: F ~> G)(implicit G: MonadBind[G]): G[A] = {
    val result: G[Any] =
      G.tailRecM[FreeMonadReflect[S, F, Any], Any](this.asInstanceOf[FreeMonadReflect[S, F, Any]]) {
        case FreeMonadReflect.Pure(a) =>
          G.pure(Right[FreeMonadReflect[S, F, Any], Any](a.asInstanceOf[Any]))
        case FreeMonadReflect.Seq(nv, s) =>
          G.fmap(trans(nv))(b => Left[FreeMonadReflect[S, F, Any], Any](run(b, s)))
      }
    result.asInstanceOf[G[A]]
  }
}

object FreeMonadReflect {
  case class Pure[S[_], F[_], A](a: A) extends FreeMonadReflect[S, F, A]
  case class Seq[S[_], F[_], A](value: F[A], continuations: S[Any => FreeMonadReflect[S, F, Any]]) extends FreeMonadReflect[S, F, A]
}

