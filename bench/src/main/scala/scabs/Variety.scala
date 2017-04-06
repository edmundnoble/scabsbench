package scabs

import cats.Eq

abstract class Variety[Typeclass[_[_]]](val name: String) {
  type Type[_]
  implicit val instance: Typeclass[Type]
}

object Variety {

  type Aux[Typeclass[_[_]], Type0[_]] = Variety[Typeclass] {
    type Type[A] = Type0[A]
  }

  implicit def varietyEq[C[_[_]]]: Eq[Variety[C]] = Util.eqByRef

  def apply[Typeclass[_[_]], Type0[_]](name: String)
                                      (implicit instance0: Typeclass[Type0]): Variety[Typeclass] =
    new Variety[Typeclass](name) {
    type Type[A] = Type0[A]
    implicit val instance: Typeclass[Type] = instance0
  }
}
