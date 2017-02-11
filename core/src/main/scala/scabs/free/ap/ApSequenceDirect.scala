package scabs.free.ap

//import scabs.Util._
//import scabs.free.Constraint.{FreeApplicative, FreeConstraint1}
//import scabs.free.ap.ApSequenceDirect.Curried
//import scabs.seq.{Sequence, StdlibInstances, TASequence}
//import shapeless._
//import scabs.seq.StdlibInstances._
//import cats.syntax.either._
//
//trait ApSequenceDirect[S[_], F[_], A] {
//  implicit val gen = ApSequenceDirect.freeApSequenceDirect[F, S]
//  import gen.generated
//  type U
//  val value: F[U] Either U
//  val mapApStack: TASequence[S, SteticC[Curried[S, F]#l]#l, U, A]
//  def retract(implicit S: Sequence[S], F: Applicative[F]): F[A] =
//    Evaluable.tailRecEval[SteticC[Curried[S, F]#l]#l, S, F, U, A](value.fold[F[U]](identity, F.pure(_)), mapApStack)(S, StdlibInstances.steticEvaluable[Curried[S, F]#l])
//  def foldMap[G[_]](trans: F ~> G)(implicit S: Sequence[S], G: Applicative[G]): G[A] =
//    ApSequenceDirect.hfunctor[S].transform(trans)(this).retract
//
//}
//
//object ApSequenceDirect {
//
//  type Curried[S[_], F[_]] = {type l[A] = ApSequenceDirect[S, F, A]}
//  type CurriedC[S[_]] = {type l[F[_], A] = ApSequenceDirect[S, F, A]}
//
//  implicit def freeApSequenceDirect[F[_], S[_]](implicit S: Sequence[S]): FreeApplicative[F, Curried[S, F]#l] =
//    new FreeConstraint1[Applicative, F, Curried[S, F]#l] {
//      override implicit val generated: Applicative[Curried[S, F]#l] = new Applicative[Curried[S, F]#l] {
//        override def pure[A](a: A): ApSequenceDirect[S, F, A] = new ApSequenceDirect[S, F, A] {
//          type U = A
//          override val value = Right(a)
//          override val mapApStack: TASequence[S, SteticC[F]#l, A, A] =
//            TASequence.empty[S, SteticC[F]#l, A]
//        }
//
//        override def ap[A, B](fa: ApSequenceDirect[S, F, A])(f: ApSequenceDirect[S, F, (A) => B]): ApSequenceDirect[S, F, B] = new ApSequenceDirect[S, F, B] {
//          type U = fa.U
//          override val value: Either[F[fa.U], fa.U] = fa.value
//          override val mapApStack: TASequence[S, SteticC[Curried[S, F]#l]#l, U, B] = fa.mapApStack andThen Left(f)
//        }
//
//        override def traverse[T[_] : Traverse, A, B](fa: T[A])(f: (A) => ApSequenceDirect[S, F, B]): ApSequenceDirect[S, F, T[B]] =
//          Traverse[T].traverse[Curried[S, F]#l, A, B](fa)(f)
//
//        override def sequence[T[_] : Traverse, A](fa: T[ApSequenceDirect[S, F, A]]): ApSequenceDirect[S, F, T[A]] =
//          Traverse[T].sequence[Curried[S, F]#l, A](fa)
//
//      }
//
//      def foldMap[A, G[_]](fv: ApSequenceDirect[S, F, A])(trans: F ~> G)(implicit ev: Applicative[G]): G[A] =
//        fv.foldMap(trans)
//
//      def retract[A](fv: ApSequenceDirect[S, F, A])(implicit ev: Applicative[F]): F[A] =
//        fv.retract
//
//    }
//
//  implicit def hfunctor[S[_]](implicit S: Sequence[S]): HFunctor[CurriedC[S]#l] = new HFunctor[CurriedC[S]#l] {
//    override def transform[G[_], H[_], A](trans: ~>[G, H])(fga: ApSequenceDirect[S, G, A]): ApSequenceDirect[S, H, A] =
//      new ApSequenceDirect[S, H, A] {
//        override type U = fga.U
//        override val value: Either[H[U], U] = fga.value.left.map(trans(_))
//        override val mapApStack: TASequence[S, SteticC[Curried[S, H]#l]#l, U, A] =
//          fga.mapApStack.transform[SteticC[Curried[S, H]#l]#l](new (SteticC[Curried[S, G]#l]#l ~~> SteticC[Curried[S, H]#l]#l) {
//            override def apply[X, B](fa: Either[ApSequenceDirect[S, G, (X) => B], (X) => B]): Either[ApSequenceDirect[S, H, (X) => B], (X) => B] =
//              fa.left.map(transform(trans)(_))
//          })
//      }
//  }
//
//  def lift[S[_], F[_], A](fa: F[A])(implicit S: Sequence[S]): ApSequenceDirect[S, F, A] = new ApSequenceDirect[S, F, A] {
//    override type U = A
//    override val value: scala.Either[F[A], A] = Left(fa)
//    override val mapApStack: TASequence[S, SteticC[ApSequenceDirect.Curried[S, F]#l]#l, A, A] =
//      TASequence.empty[S, SteticC[Curried[S, F]#l]#l, A]
//  }
//
//}
