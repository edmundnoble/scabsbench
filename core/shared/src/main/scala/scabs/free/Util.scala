package scabs.free

import scabs.colls.Sequence

import scala.annotation.tailrec

object Util {

  @tailrec
  def seqRecurse[S[_]](v: Any, funs: S[Any => Any])(implicit S: Sequence[S]): Any = {
    S.uncons(funs) match {
      case Some((head, tail)) =>
        seqRecurse(head(v), tail)
      case None =>
        v
    }
  }

}
