package scabs.free.ap

import scabs.Util.{Applicative, ~>}

// From https://www.eyrie.org/~zednenem/2013/05/27/freeapp

object ApList {
  private val nil: ApList[Nothing, Unit] = Nil[Nothing]()
  def empty[F[_]]: ApList[F, Unit] = nil.asInstanceOf[ApList[F, Unit]]
}

sealed trait ApList[F[_], A] {
  def reduce(implicit F: Applicative[F]): F[A]
  def reduceMap[G[_]](trans: F ~> G)(implicit G: Applicative[G]): G[A]
}
private case class Nil[F[_]]() extends ApList[F, Unit] {
  def reduce(implicit F: Applicative[F]): F[Unit] = F.pure(())
  def reduceMap[G[_]](trans: F ~> G)(implicit G: Applicative[G]): G[Unit] = G.pure(())
}
case class Cons[F[_], A, U](fa: F[A], subl: ApList[F, U]) extends ApList[F, (A, U)] {
  def reduce(implicit F: Applicative[F]): F[(A, U)] = F.tuple2(fa, subl.reduce)
  def reduceMap[G[_]](trans: F ~> G)(implicit G: Applicative[G]): G[(A, U)] = G.tuple2(trans(fa), subl.reduceMap(trans))
}

