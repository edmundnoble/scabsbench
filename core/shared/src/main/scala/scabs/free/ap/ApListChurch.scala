package scabs.free.ap

import scabs.Util.{Applicative, ~>}

trait Eliminator[F[_], Y, Z] { self =>
  def eliminate[X](fun: X => Y, seq: ApList[F, X]): Z
  def contramap[Y2](and: Y2 => Y): Eliminator[F, Y2, Z] = new Eliminator[F, Y2, Z] {
    override def eliminate[X](fun: (X) => Y2, seq: ApList[F, X]): Z = self.eliminate[X](x => and(fun(x)), seq)
  }
}

trait ApListChurch[F[_], A] {
  def elim[U, Y, Z](eliminator: Eliminator[F, Y, Z], folder: (U, A) => Y, aps: ApList[F, U]): Z
}

object ApListChurch {
  def lift[F[_], A](fa: F[A]): ApListChurch[F, A] = new ApListChurch[F, A] {
    override def elim[U, Y, Z](eliminator: Eliminator[F, Y, Z], folder: (U, A) => Y, aps: ApList[F, U]): Z = {
      eliminator.eliminate[(A, U)]({ case (a, s) => folder(s, a) }, Cons(fa, aps))
    }
  }

  def retract[F[_], A](fa: ApListChurch[F, A])(implicit F: Applicative[F]): F[A] = {
    fa.elim[Unit, A, F[A]](new Eliminator[F, A, F[A]] {
      override def eliminate[X](fun: (X) => A, seq: ApList[F, X]): F[A] = F.fmap(seq.reduce)(fun)
    }, { case ((), a) => a }, ApList.empty[F])
  }

  def foldMap[F[_], G[_], A](fa: ApListChurch[F, A], trans: F ~> G)(implicit G: Applicative[G]): G[A] = {
    fa.elim[Unit, A, G[A]](new Eliminator[F, A, G[A]] {
      override def eliminate[X](fun: (X) => A, seq: ApList[F, X]): G[A] = G.fmap(seq.reduceMap(trans))(fun)
    }, { case ((), a) => a }, ApList.empty[F])
  }

  def fmap[F[_], A, B](x: ApListChurch[F, A])(g: A => B): ApListChurch[F, B] = {
    new ApListChurch[F, B] {
      override def elim[U, Y, Z](k: Eliminator[F, Y, Z], f: (U, B) => Y, aps: ApList[F, U]): Z = {
        x.elim[U, Y, Z](k, (u, b) => f(u, g(b)), aps)
      }
    }
  }

  def pure[F[_], A](v: A): ApListChurch[F, A] = new ApListChurch[F, A] {
    override def elim[U, Y, Z](eliminator: Eliminator[F, Y, Z], folder: (U, A) => Y, aps: ApList[F, U]): Z = {
      eliminator.eliminate[U](folder(_, v), aps)
    }
  }

  def ap[F[_], A, B](x: ApListChurch[F, A])(g: ApListChurch[F, A => B]): ApListChurch[F, B] = {
    new ApListChurch[F, B] {
      override def elim[U, Y, Z](eliminator: Eliminator[F, Y, Z], folder: (U, B) => Y, aps: ApList[F, U]): Z = {
        // apply x y = freeAp (\k f -> unFreeAp y (unFreeAp x k) (\s a g -> f s (g a)))
        g.elim[U, (Y, A), Z](new Eliminator[F, (Y, A), Z] {
          override def eliminate[X](fun: (X) => (Y, A), seq: ApList[F, X]): Z =
            x.elim[X, (Y, A), Z](eliminator.contramap(_._1), (u, a) => fun(u), seq)
        }, (u, ab) => ???, aps)
      }
    }
  }

}
