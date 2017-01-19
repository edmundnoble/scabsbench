package scabs.free.monad

import scabs.Util.{Monad, ~>}
import scabs.seq.Sequence

import scala.annotation.tailrec

sealed trait RWR[S[_], F[_], A] {
  @tailrec
  final def run(v: Any, nexts: S[Any => RWR[S, F, Any]])(implicit S: Sequence[S]): RWR[S, F, Any] = S.uncons(nexts) match {
    case None => RWR.Pure(v)
    case Some((head, tail)) =>
      head(v) match {
        case RWR.Pure(a) => run(a, tail)
        case RWR.Seq(nv, s) => RWR.Seq[S, F, Any](nv, S.concat(s, tail))
      }
  }

  def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G], S: Sequence[S]): G[A] = {
    val result: G[Any] =
      G.tailRecM[RWR[S, F, Any], Any](this.asInstanceOf[RWR[S, F, Any]]) {
        case RWR.Pure(a) =>
          G.pure(Right[RWR[S, F, Any], Any](a.asInstanceOf[Any]))
        case RWR.Seq(nv, s) =>
          G.fmap(trans(nv))(b => Left[RWR[S, F, Any], Any](run(b, s)))
      }
    result.asInstanceOf[G[A]]
  }
}

object RWR {
  case class Pure[S[_], F[_], A](a: A) extends RWR[S, F, A]
  case class Seq[S[_], F[_], A](value: F[A], continuations: S[Any => RWR[S, F, Any]]) extends RWR[S, F, A]
}

