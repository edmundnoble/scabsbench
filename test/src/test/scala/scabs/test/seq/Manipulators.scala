package scabs.test.seq

import org.scalacheck.Gen
import scabs.seq.Sequence

object Manipulators {

  def arbitrarySequence[F[_], A](implicit A: Gen[A], F: Sequence[F]): Gen[F[A]] =
    Gen.chooseNum(0, 5).flatMap { i =>
      def loop(c: Int, s: Gen[F[A]]): Gen[F[A]] =
        if (c == 0) {
          s
        } else {
          s.flatMap(fa => A.map(F.cons(_, fa)))
        }

      loop(i, Gen.const(F.empty[A]))
    }

}
