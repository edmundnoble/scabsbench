package scabs.free.ap

import scabs.Util.{Applicative, ~>}

sealed trait Tagless[F[_], A] {
  def foldMap[G[_] : Applicative](trans: F ~> G): G[A]

  def retract(implicit F: Applicative[F]): F[A]
}

object Tagless {

  type Curried[F[_]] = { type l[A] = Tagless[F, A] }

  implicit def taglessApplicative[F[_]]: Applicative[Curried[F]#l] = new Applicative[Curried[F]#l] {
    override def pure[A](a: A): Tagless[F, A] = Tagless.pure(a)

    override def ap[A, B](fa: Tagless[F, A])(f: Tagless[F, (A) => B]): Tagless[F, B] = Tagless.ap(fa)(f)

    override def map2[A, B, C](fa: Tagless[F, A], fb: Tagless[F, B])(f: (A, B) => C): Tagless[F, C] = Tagless.map2(fa, fb)(f)

    override def tuple2[A, B](fa: Tagless[F, A], fb: Tagless[F, B]): Tagless[F, (A, B)] = Tagless.map2(fa, fb)((a, b) => (a, b))

    override def fmap[A, B](fa: Tagless[F, A])(f: (A) => B): Tagless[F, B] = Tagless.fmap(fa)(f)
  }

  def map2[F[_], A, B, C](fa: Tagless[F, A], fb: Tagless[F, B])(f: (A, B) => C): Tagless[F, C] =
    new Tagless[F, C] {
      override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[C] =
        ev.map2(fa.foldMap(trans), fb.foldMap(trans))(f)

      override def retract(implicit ev: Applicative[F]): F[C] =
        ev.map2(fa.retract, fb.retract)(f)
    }

  def ap[F[_], A, B](fa: Tagless[F, A])(ff: Tagless[F, A => B]): Tagless[F, B] = new Tagless[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] =
      ev.ap(fa.foldMap(trans))(ff.foldMap(trans))

    override def retract(implicit ev: Applicative[F]): F[B] =
      ev.ap(fa.retract)(ff.retract)
  }

  def pure[F[_], A](a: A): Tagless[F, A] = new Tagless[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[A] = ev.pure(a)

    override def retract(implicit ev: Applicative[F]): F[A] = ev.pure(a)
  }

  def inj[F[_], A](fa: F[A]): Tagless[F, A] = new Tagless[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[A] = trans(fa)

    override def retract(implicit ev: Applicative[F]): F[A] = fa
  }

  def fmap[F[_], A, B](frf: Tagless[F, A])(f: A => B): Tagless[F, B] = new Tagless[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit ev: Applicative[G]): G[B] = ev.fmap(frf.foldMap(trans))(f)

    override def retract(implicit ev: Applicative[F]): F[B] = ev.fmap(frf.retract)(f)
  }

}
