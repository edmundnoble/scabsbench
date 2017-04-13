package scabs
package free
package ap

import scabs.Util.{Applicative, Traverse, ~>}
import scabs.free.Constraint.{FreeApplicative, FreeConstraint1}

sealed trait Tagless[F[_], A] {
  def foldMap[G[_] : Applicative](trans: F ~> G): G[A]

  def retract(implicit F: Applicative[F]): F[A]
}

object Tagless {

  type Curried[F[_]] = {type l[A] = Tagless[F, A]}

  def map2[F[_], A, B, C](fa: Tagless[F, A], fb: Tagless[F, B])(f: (A, B) => C): Tagless[F, C] =
    new Tagless[F, C] {
      def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[C] =
        ev.map2(fa.foldMap(trans), fb.foldMap(trans))(f)

      def retract(implicit ev: Applicative[F]): F[C] =
        ev.map2(fa.retract, fb.retract)(f)
    }

  def ap[F[_], A, B](fa: Tagless[F, A])(ff: Tagless[F, A => B]): Tagless[F, B] = new Tagless[F, B] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] =
      ev.ap(fa.foldMap(trans))(ff.foldMap(trans))

    def retract(implicit ev: Applicative[F]): F[B] =
      ev.ap(fa.retract)(ff.retract)
  }

  def pure[F[_], A](a: A): Tagless[F, A] = new Tagless[F, A] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[A] = ev.pure(a)

    def retract(implicit ev: Applicative[F]): F[A] = ev.pure(a)
  }

  def inj[F[_], A](fa: F[A]): Tagless[F, A] = new Tagless[F, A] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[A] = trans(fa)

    def retract(implicit ev: Applicative[F]): F[A] = fa
  }

  def fmap[F[_], A, B](frf: Tagless[F, A])(f: A => B): Tagless[F, B] = new Tagless[F, B] {
    def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] = ev.fmap(frf.foldMap(trans))(f)

    def retract(implicit ev: Applicative[F]): F[B] = ev.fmap(frf.retract)(f)
  }

  implicit def freeApplicativeTagless[F[_]]: FreeApplicative[F, Curried[F]#l] =
    new FreeConstraint1[Applicative, F, Curried[F]#l] {
      implicit val generated: Applicative[Curried[F]#l] = new Applicative[Curried[F]#l] {
        def pure[A](a: A): Tagless[F, A] =
          Tagless.pure(a)

        def ap[A, B](fa: Tagless[F, A])(ff: Tagless[F, A => B]): Tagless[F, B] =
          Tagless.ap(fa)(ff)

        def traverse[S[_] : Traverse, A, B](fa: S[A])(f: (A) => Tagless[F, B]): Tagless[F, S[B]] =
          Traverse[S].traverse[Curried[F]#l, A, B](fa)(f)

        def sequence[S[_] : Traverse, A](fa: S[Tagless[F, A]]): Tagless[F, S[A]] =
          Traverse[S].sequence[Curried[F]#l, A](fa)
      }

      def foldMap[A, G[_]](fv: Tagless[F, A])(trans: F ~> G)(implicit ev: Applicative[G]): G[A] =
        fv.foldMap(trans)

      def retract[A](fv: Tagless[F, A])(implicit ev: Applicative[F]): F[A] =
        fv.retract

      def lift[A](a: F[A]): Tagless[F, A] = Tagless.inj(a)
    }

}
