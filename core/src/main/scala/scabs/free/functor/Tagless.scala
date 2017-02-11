package scabs
package free
package functor

import scabs.Util.{Functor, Traverse, ~>}
import scabs.free.Constraint.{FreeConstraint1, FreeFunctor}

trait Tagless[F[_], A] {
  def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[A]
  def retract(implicit F: Functor[F]): F[A]
}

object Tagless {
  type Curried[F[_]] = {type l[A] = Tagless[F, A]}
  def fmap[F[_], A, B](self: Tagless[F, A])(fun: A => B): Tagless[F, B] = new Tagless[F, B] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[B] =
      G.fmap(self.foldMap(trans))(fun)
    override def retract(implicit F: Functor[F]): F[B] =
      F.fmap(self.retract)(fun)
  }
  def lift[F[_], A](value: F[A]): Tagless[F, A] = new Tagless[F, A] {
    override def foldMap[G[_]](trans: ~>[F, G])(implicit G: Functor[G]): G[A] =
      trans(value)
    override def retract(implicit F: Functor[F]): F[A] =
      value
  }

  implicit def freeFunctorTagless[F[_]]: FreeFunctor[F, Curried[F]#l] = new FreeConstraint1[Functor, F, Curried[F]#l] {
    override val generated: Functor[Curried[F]#l] = new Functor[Curried[F]#l] {
      override def fmap[A, B](fa: Tagless[F, A])(f: (A) => B): Tagless[F, B] =
        Tagless.fmap(fa)(f)

      override def tailRecF[A, B](fa: Tagless[F, A])(f: (A) => Either[A, B]): Tagless[F, B] = {
        def loop(a: A): B = f(a).fold(loop, identity)
        fmap(fa)(loop)
      }
    }

    override def foldMap[A, G[_]](fv: Tagless[F, A])(trans: F ~> G)(implicit ev: Functor[G]): G[A] =
      fv.foldMap(trans)

    override def retract[A](fv: Tagless[F, A])(implicit ev: Functor[F]): F[A] =
      fv.retract

  }
}

