package scabs

import cats.Eq
import org.scalacheck.Prop
import org.scalacheck.util.Pretty

import scala.language.implicitConversions

sealed abstract class Assertion

final case class Equal[A](f: A, s: A)(implicit val toPretty: A => Pretty) extends Assertion

final case class StackOverflow[A](eval: () => A) extends Assertion

final case class StackSafe[A](eval: () => A) extends Assertion

final case class And(assertions: Seq[Assertion]) extends Assertion

object Assertion {

  trait DSL {
    def equal[A](f: A, s: A): Assertion = {
      Equal(f, s)
    }

    def stackOverflow[A](a: => A): Assertion = {
      StackOverflow(() => a)
    }

    def stackSafe[A](a: => A): Assertion = {
      StackOverflow(() => a)
    }

    def and(as: Assertion*): Assertion = {
      And(as)
    }
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
      case Equal(f, s) =>
        f == s
      case StackOverflow(f) =>
        overflowsStack(f)
      case StackSafe(f) =>
        !overflowsStack(f)
      case And(assertions) =>
        assertions.forall(resultOf)
    }
  }

  implicit def assertionToProp(assertion: Assertion): Prop = {
    assertion match {
      case eq@Equal(f, s) => Prop.=?(f, s)(eq.toPretty)
      case StackOverflow(f) =>
        if (overflowsStack(f))
          Prop.proved
        else Prop.falsified :| {
          "Expected a stack overflow"
        }
      case StackSafe(f) =>
        if (!overflowsStack(f))
          Prop.proved
        else Prop.falsified :| {
          "Unexpected stack overflow"
        }
      case And(assertions) =>
        Prop.all(assertions.map(assertionToProp): _*)
    }
  }

}
