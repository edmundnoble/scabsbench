package scabs.free.monad

sealed trait RWRPlusMap[S[_], F[_], A] {
  @tailrec
  final def run(v: Any, nexts: S[Any => RWRPlusMap[S, F, Any]])(implicit S: Sequence[S]): RWRPlusMap[S, F, Any] =
    S.uncons(nexts) match {
    case None => RWRPlusMap.Pure(v)
    case Some((head, tail)) =>
      head(v) match {
        case RWRPlusMap.Pure(a) => run(a, tail)
        case RWRPlusMap.Seq(nv, s) => RWRPlusMap.Seq[S, F, Any](nv, S.concat(s, tail))
        case RWRPlusMap.Map(nv, s) =>
          RWRPlusMap.Seq[S, F, Any](nv,
            S.cons((a: Any) => RWRPlusMap.Pure(Util.seqRecurse(a, s)), tail))
      }
  }

  def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G], S: Sequence[S]): G[A] = {
    val result: G[Any] =
      G.tailRecM[RWRPlusMap[S, F, Any], Any](this.asInstanceOf[RWRPlusMap[S, F, Any]]) {
        case RWRPlusMap.Pure(a) =>
          G.pure(Right[RWRPlusMap[S, F, Any], Any](a.asInstanceOf[Any]))
        case RWRPlusMap.Seq(nv, s) =>
          G.fmap(trans(nv))(b => Left[RWRPlusMap[S, F, Any], Any](run(b, s)))
        case RWRPlusMap.Map(nv, maps) =>
          G.fmap(trans(nv))(b => Right[RWRPlusMap[S, F, Any], Any](Util.seqRecurse[S](b, maps)))
      }
    result.asInstanceOf[G[A]]
  }
}

object RWRPlusMap {
  case class Pure[S[_], F[_], A](a: A) extends RWRPlusMap[S, F, A]
  case class Seq[S[_], F[_], A](value: F[A], continuations: S[Any => RWRPlusMap[S, F, Any]]) extends RWRPlusMap[S, F, A]
  case class Map[S[_], F[_], A](value: F[A], maps: S[Any => Any]) extends RWRPlusMap[S, F, A]
}

