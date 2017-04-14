package scabs
package free

import cats.{Monad, Applicative}
import scabs.seq.Sequence

object Util {

  def seqRecurse[S[_]](v: Any, funs: S[Any => Any])(implicit S: Sequence[S]): Any =
    S.fold[Any => Any, Any](funs)(v)((a, f) => f(a))

  // TODO: finish with tailRecM
  def monadRecurse[F[_], S[_]](v: F[Any], funs: S[Any => F[Any]])(implicit F: Monad[F], S: Sequence[S]): Any =
    S.fold(funs)(v)(F.flatMap(_)(_))

  def apRecurse[F[_], S[_]](v: F[Any], funs: S[F[Any => Any]])(implicit F: Applicative[F], S: Sequence[S]): Any =
    S.fold(funs)(v)((a, f) => F.ap(f)(a))

}
