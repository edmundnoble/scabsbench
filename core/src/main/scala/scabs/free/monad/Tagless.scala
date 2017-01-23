package scabs
package free
package monad

import scabs.Util.{Monad, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeMonad}

sealed trait Tagless[F[_], A] {
  def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A]

  def retract(implicit ev: Monad[F]): F[A]
}

object Tagless {
  type Curried[F[_]] = {type l[A] = Tagless[F, A]}

  implicit def taglessFreeMonad[F[_]]: FreeMonad[F, Curried[F]#l] = new FreeConstraint1[Monad, F, Curried[F]#l] {
    override val generated: Monad[Curried[F]#l] = new Monad[Curried[F]#l] {
      override def pure[A](a: A): Tagless[F, A] =
        Tagless.pure(a)

      override def fmap[A, B](fa: Tagless[F, A])(f: (A) => B): Tagless[F, B] =
        Tagless.fmap(fa)(f)

      override def bind[A, B](fa: Tagless[F, A])(f: (A) => Tagless[F, B]): Tagless[F, B] =
        Tagless.bind(fa)(f)

      override def join[A](ffa: Tagless[F, Tagless[F, A]]): Tagless[F, A] =
        Tagless.join(ffa)

      override def tailRecM[A, B](a: A)(f: (A) => Tagless[F, Either[A, B]]): Tagless[F, B] =
        Tagless.bind(f(a))(_.fold(tailRecM(_)(f), pure))
    }

    override def foldMap[A, G[_]](fv: Tagless[F, A])(trans: ~>[F, G])(implicit ev: Monad[G]): G[A] =
      fv.foldMap(trans)

    override def retract[A](fv: Tagless[F, A])(implicit ev: Monad[F]): F[A] =
      fv.retract
  }

  def pure[F[_], A](a: A): Tagless[F, A] = new Tagless[F, A] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] =
      G.pure(a)

    override def retract(implicit F: Monad[F]): F[A] =
      F.pure(a)
  }

  def fmap[F[_], A, B](fa: Tagless[F, A])(f: A => B): Tagless[F, B] = new Tagless[F, B] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[B] =
      G.fmap(fa.foldMap(trans))(f)

    override def retract(implicit F: Monad[F]): F[B] =
      F.fmap(fa.retract)(f)
  }

  def lift[F[_], A](alg: F[A]): Tagless[F, A] = new Tagless[F, A] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] =
      trans(alg)

    override def retract(implicit ev: Monad[F]): F[A] =
      alg
  }

  def bind[F[_], A, B](fa: Tagless[F, A])(f: A => Tagless[F, B]): Tagless[F, B] = new Tagless[F, B] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[B] =
      G.bind(fa.foldMap(trans))(f(_).foldMap(trans))

    override def retract(implicit F: Monad[F]): F[B] =
      F.bind(fa.retract)(f(_).retract)
  }

  def join[F[_], A](ffa: Tagless[F, Tagless[F, A]]): Tagless[F, A] = new Tagless[F, A] {
    override def foldMap[G[_]](trans: F ~> G)(implicit G: Monad[G]): G[A] =
      G.bind(ffa.foldMap(trans))(_.foldMap(trans))

    override def retract(implicit F: Monad[F]): F[A] =
      F.bind(ffa.retract)(_.retract)
  }

}
