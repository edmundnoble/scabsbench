package scabs.test.seq

import cats.laws.discipline.arbitrary
import cats.~>
import org.scalacheck.{Arbitrary, Gen, Shrink}
import scabs.seq.Sequence

/**
  *
  *
  * @author Matthew Pocock
  */
sealed trait StackOps[T] {
  final def replay[S[_]: Sequence]: S[T] =
    StackOps.replay[S, T](this)
}

object StackOps {
  case class Empty[T]() extends StackOps[T]
  final case class Cons[T](head: T, ops: StackOps[T]) extends StackOps[T]
  final case class Snoc[T](last: T, ops: StackOps[T]) extends StackOps[T]
  final case class Append[T](lhs: StackOps[T], rhs: StackOps[T]) extends StackOps[T]

  implicit def genEmpty[T]: Gen[Empty[T]] = Gen.const(Empty())

  implicit def genCons[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] = Gen.sized { size =>
    if(size <= 0) Empty[T]()
    else for {
      head <- t.arbitrary
      g = Gen.resize(size - 1, genOps[T])
      ops <- g
    } yield Cons(head, ops)
  }

  implicit def genSnoc[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] = Gen.sized { size =>
    if(size <= 0) Empty[T]()
    else for {
      last <- t.arbitrary
      g = Gen.resize(size - 1, genOps[T])
      ops <- g
    } yield Snoc(last, ops)
  }

  implicit def genAppend[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] = Gen.sized { size =>
    if (size <= 0) Empty[T]()
    else for {
      ls <- Gen.chooseNum(0, size-1)
      rs = size - ls
      lhs <- Gen.resize(ls, genOps[T])
      rhs <- Gen.resize(rs, genOps[T])
    } yield Append(lhs, rhs)
  }

  implicit def genOps[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] =
    Gen.oneOf(genCons[T], genSnoc[T], genAppend[T])

  implicit def shrinkOps[T]: Shrink[StackOps[T]] = Shrink[StackOps[T]] {
    case Empty() => Stream()
    case Cons(_, ops) => Stream(ops)
    case Snoc(_, ops) => Stream(ops)
    case Append(Empty(), Empty()) => Stream(Empty())
    case Append(Empty(), rhs) => Stream(rhs)
    case Append(lhs, Empty()) => Stream(lhs)
    case Append(lhs, rhs) => Stream(lhs, rhs, Empty())
  }

  def replay[Rep[_], T](ops: StackOps[T])(implicit s: Sequence[Rep]): Rep[T] = ops match {
    case StackOps.Empty() => s.empty
    case StackOps.Cons(h, o) => s.cons(h, replay(o))
    case StackOps.Snoc(h, o) => s.snoc(replay(o), h)
    case StackOps.Append(l, r) => s.concat(replay(l), replay(r))
  }
}
