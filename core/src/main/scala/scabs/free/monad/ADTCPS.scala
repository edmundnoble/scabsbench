package scabs
package free
package monad

import scabs.Util._
import scabs.free.Constraint.FreeMonad

sealed trait ADTCPS[F[_], A] {
  def foldMap[G[_] : Monad](trans: F ~> G): G[A]
  def retract(implicit F: Monad[F]): F[A]
}

object ADTCPS {
  type Curried[F[_]] = { type l[A] = ADTCPS[F, A] }
  implicit def adtCpsFreeMonad[F[_]]: FreeMonad[F, Curried[F]#l] = new FreeMonad[F, Curried[F]#l] {
    val generated: Monad[Curried[F]#l] = new Monad[Curried[F]#l] {
      override def pure[A](a: A): ADTCPS[F, A] = ADTCPS.Pure(a)

      override def fmap[A, B](fa: ADTCPS[F, A])(f: (A) => B): ADTCPS[F, B] =
        bind(fa)(f.andThen(ADTCPS.Pure(_)))

      override def bind[A, B](fa: ADTCPS[F, A])(f: (A) => ADTCPS[F, B]): ADTCPS[F, B] =
        ADTCPS.Bind(fa, f)

      override def join[A](ffa: ADTCPS[F, ADTCPS[F, A]]): ADTCPS[F, A] =
        bind(ffa)(fa => fa)

      override def tailRecM[A, B](a: A)(f: (A) => ADTCPS[F, Either[A, B]]): ADTCPS[F, B] =
        bind(f(a))(_.fold(tailRecM(_)(f), pure))
    }
    def foldMap[A, G[_]](fv: ADTCPS[F, A])(trans: F ~> G)(implicit ev: Monad[G]): G[A] = fv.foldMap(trans)
    def retract[A](fv: ADTCPS[F, A])(implicit ev: Monad[F]): F[A] = fv.retract
  }
  case class Pure[F[_], A](value: A) extends ADTCPS[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.pure(value)
    override def retract(implicit F: Monad[F]): F[A] =
      F.pure(value)
  }
  case class Bind[F[_], A, B](free: ADTCPS[F, A], bind: A => ADTCPS[F, B]) extends ADTCPS[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[B] =
      G.bind(free.foldMap(trans))(bind.andThen(_.foldMap(trans)))
    override def retract(implicit F: Monad[F]): F[B] =
      F.bind(free.retract)(bind(_).retract)
  }
}

