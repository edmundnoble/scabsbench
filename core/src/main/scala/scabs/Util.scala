package scabs

import cats.{Eq, Id, ~>}
import scabs.seq.{Sequence, TASequence}
import simulacrum.{op, typeclass}

import scala.annotation.tailrec

object Util {

  type Const[A] = {type l[B] = A}
  type BiConst[A] = {type l[B, C] = A}
  type ~~>[F[_, _], G[_, _]] = BinatTrans[F, G]
  type <~>[F[_], G[_]] = (F ~> G, G ~> F)
  type Kleisli[F[_], A, B] = A => F[B]
  type KleisliC[F[_]] = {type l[A, B] = A => F[B]}
  type Static[F[_], A, B] = F[A => B]
  type StaticC[F[_]] = {type l[A, B] = F[A => B]}
  type Stetic[F[_], A, B] = F[A => B] Either (A => B)
  type SteticC[F[_]] = {type l[A, B] = F[A => B] Either (A => B)}
  type Cokleisli[F[_], A, B] = F[A] => B
  type CokleisliC[F[_]] = {type l[A, B] = F[A] => B}
  type Algebra[F[_], A] = A => F[A]
  type Coalgebra[F[_], A] = F[A] => A

  def FunctionToKleisli[F[_]](pure: Id ~> F): Function1 ~~> KleisliC[F]#l = new (Function1 ~~> KleisliC[F]#l) {
    def apply[A, B](fa: (A) => B): (A) => F[B] = fa.andThen(pure(_))
  }

  def eqByRef[A <: AnyRef]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = x eq y
  }

  trait Lub1[F[_], G[_]] {
    def ev[A]: F[A] <:< G[A]
  }

  trait BinatTrans[F[_, _], G[_, _]] {
    def apply[A, B](fa: F[A, B]): G[A, B]
  }

  trait Semigroupoid[F[_, _]] {
    def compose[A, B, C](ab: F[A, B], bc: F[B, C]): F[A, C]
  }

  trait Category[F[_, _]] {
    def compose[A, B, C](ab: F[A, B], bc: F[B, C]): F[A, C]

    def id[A]: F[A, A]
  }

  trait Evaluable[F[_, _], G[_]] {
    def eval[A, B](a: G[A], arr: F[A, B]): G[B]
  }

  object Evaluable {
    def tailRecEval[F[_, _], S[_], G[_], A, B](input: G[A], taSequence: TASequence[S, F, A, B])(implicit S: Sequence[S], C: Evaluable[F, G]): G[B] = {
      taSequence.uncons match {
        case Some((head, tail)) => tailRecEval(C.eval(input, head.asInstanceOf[F[A, Any]]), tail)
        case None => input.asInstanceOf[G[B]]
      }
    }
  }

  trait Prearrow[F[_, _]] {
    def compose[A, B, C](ab: F[A, B], bc: F[B, C]): F[A, C]

    def id[A]: F[A, A]

    def arr[A, B](f: A => B): F[A, B]
  }

  trait CategoryTailrec[F[_, _]] {
    def compose[A, B, C](ab: F[A, B], bc: F[B, C]): F[A, C]

    def id[A]: F[A, A]

    def tailRecP[S[_] : Sequence, A, B](seq: TASequence[S, F, A, B]): F[A, B]
  }

  trait HFunctor[F[_[_], _]] {
    def transform[G[_], H[_], A](trans: G ~> H)(fga: F[G, A]): F[H, A]
  }

  trait HMonad[F[_[_], _]] {
    def pures[G[_], A](ga: G[A]): F[G, A]

    def flasten[G[_], A](fgfga: F[G, F[G, A]]): F[G, A]
  }

}
