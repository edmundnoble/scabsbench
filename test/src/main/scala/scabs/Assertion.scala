package scabs

import cats.Eq

sealed abstract class Assertion

final case class Equal[A](as: Seq[A]) extends Assertion

final case class StackOverflow[A](eval: () => A) extends Assertion

final case class StackSafe[A](eval: () => A) extends Assertion

final case class And(assertions: Seq[Assertion]) extends Assertion

final case class Or(assertions: Seq[Assertion]) extends Assertion

object Assertion {

  trait DSL {
    def equal[A](as: A*): Assertion = Equal(as)

    def stackOverflow[A](a: => A): Assertion = StackOverflow(() => a)

    def stackSafe[A](a: => A): Assertion = StackOverflow(() => a)

    def and(as: Assertion*): Assertion = And(as)

    def or(as: Assertion*): Assertion = Or(as)
  }

  def allEqual[A](as: Seq[A]): Boolean = {
    if (as.isEmpty) {
      true
    } else {
      val head = as.head
      as.tail.forall(_ == head)
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

  def resultOf(assertion: Assertion): Boolean = {
    assertion match {
      case Equal(as) =>
        allEqual(as)
      case StackOverflow(f) =>
        overflowsStack(f)
      case StackSafe(f) =>
        !overflowsStack(f)
      case And(assertions) =>
        assertions.forall(resultOf)
      case Or(assertions) =>
        assertions.exists(resultOf)
    }
  }

}
