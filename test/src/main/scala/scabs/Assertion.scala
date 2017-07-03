package scabs

import cats.Eq
import org.scalacheck.Prop
import org.scalacheck.util.Pretty

import scala.language.implicitConversions

final case class Equal[A](f: A, s: A)

final case class StackOverflow[A](eval: () => A)

final case class StackSafe[A](eval: () => A)

final case class And[A, B](f: A, s: B)

trait AssertionT[A] {
  def resultOf(a: A): Prop
}

object AssertionT {
  implicit def toAssertionOps[A](a: A): AssertionTOps[A] = new AssertionTOps[A](a)

  implicit def assertionTEqualsProp[A: (? => Pretty)]: AssertionT[Equal[A]] = {
    new AssertionT[Equal[A]] {
      def resultOf(a: Equal[A]): Prop = Prop.=?(a.f, a.s)
    }
  }

  implicit def assertionTStackOverflowProp[A]: AssertionT[StackOverflow[A]] = {
    new AssertionT[StackOverflow[A]] {
      def resultOf(a: StackOverflow[A]): Prop = {
        if (overflowsStack(a.eval)) Prop.proved
        else Prop.falsified :| "Expected a stack overflow"
      }
    }
  }

  implicit def assertionTStackSafeProp[A]: AssertionT[StackSafe[A]] = {
    new AssertionT[StackSafe[A]] {
      def resultOf(a: StackSafe[A]): Prop = {
        if (!overflowsStack(a.eval)) Prop.proved
        else Prop.falsified :| "Unexpected stack overflow"
      }
    }
  }

  implicit def assertionTAndProp[A, B](implicit indA: AssertionT[A], indB: AssertionT[B]): AssertionT[And[A, B]] = {
    new AssertionT[And[A, B]] {
      def resultOf(a: And[A, B]): Prop = indA.resultOf(a.f) && indB.resultOf(a.s)
    }
  }

  def overflowsStack[A](f: () => A): Boolean = {
    try {
      f
      false
    } catch {
      case _: StackOverflowError =>
        true
    }
  }

}

final class AssertionTOps[A](val a: A) extends AnyVal {
  def toProp(implicit inst: AssertionT[A]): Prop = inst.resultOf(a)
}
