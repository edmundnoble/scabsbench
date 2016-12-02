/*
package scolls

object KaplanTarjanSteque {
  sealed trait Buffer[+A]
  object Buffer {
    case object Empty extends Buffer[Nothing]
    final case class One[A](arg1: A) extends Buffer[A]
    final case class Two[A](arg1: A, arg2: A) extends Buffer[A]
    final case class Three[A](arg1: A, arg2: A, arg3: A) extends Buffer[A]
  }
  sealed trait Color
  object Color {
    case object Green extends Color
    case object Yellow extends Color
    case object Red extends Color
  }
}

import KaplanTarjanSteque._

final case class KaplanTarjanSteque[A <: AnyRef](prefix: Buffer[A], child: KaplanTarjanSteque[(A, A)], suffix: Buffer[A]) {
  def empty: Boolean = prefix != Buffer.Empty
  def nonEmpty: Boolean = false
  def push(a: A): KaplanTarjanSteque[A] = if (prefix._3 == null) {
    copy(child = child.push(a))
  } else {

  }
  def pop: (A, KaplanTarjanSteque[A]) = if (prefix._1 == null && child.nonEmpty) {
    copy(child = child.push(a))
  } else {

  }
}
*/
