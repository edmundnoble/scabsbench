package scabs
package free
package monad

import cats._
import cats.implicits._
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

      override def map[A, B](fa: ADTCPS[F, A])(f: (A) => B): ADTCPS[F, B] =
        flatMap(fa)(f.andThen(ADTCPS.Pure(_)))

      override def flatMap[A, B](fa: ADTCPS[F, A])(f: (A) => ADTCPS[F, B]): ADTCPS[F, B] =
        ADTCPS.Bind(fa, f)

      override def flatten[A](ffa: ADTCPS[F, ADTCPS[F, A]]): ADTCPS[F, A] =
        flatMap(ffa)(fa => fa)

      override def tailRecM[A, B](a: A)(f: (A) => ADTCPS[F, Either[A, B]]): ADTCPS[F, B] =
        flatMap(f(a))(_.fold(tailRecM(_)(f), pure))
    }
    def foldMap[A, G[_]](fv: ADTCPS[F, A])(trans: F ~> G)(implicit ev: Monad[G]): G[A] = fv.foldMap(trans)
    def retract[A](fv: ADTCPS[F, A])(implicit ev: Monad[F]): F[A] = fv.retract
    def lift[A](a: F[A]): ADTCPS[F, A] = ADTCPS.Lift(a)
  }
  case class Pure[F[_], A](value: A) extends ADTCPS[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      G.pure(value)
    override def retract(implicit F: Monad[F]): F[A] =
      F.pure(value)
  }
  case class Lift[F[_], A](value: F[A]) extends ADTCPS[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[A] =
      trans(value)
    override def retract(implicit F: Monad[F]): F[A] =
      value
  }
  case class Bind[F[_], A, B](free: ADTCPS[F, A], flatMap: A => ADTCPS[F, B]) extends ADTCPS[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Monad[G]): G[B] =
      G.flatMap(free.foldMap(trans))(flatMap.andThen(_.foldMap(trans)))
    override def retract(implicit F: Monad[F]): F[B] =
      F.flatMap(free.retract)(flatMap(_).retract)
  }
}

