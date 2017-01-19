package scabs

import org.scalameter.api.Gen
import scabs.Util.Const

final case class TCBenchVariety[C[_[_]], M[_]](name: String)
                                              (implicit val instance: C[M]) {
  def forget: TCBenchVariety[C, Nothing] =
    this.asInstanceOf[TCBenchVariety[C, Nothing]]
}

abstract class TCBenchmark[C[_[_]], M[_], I] extends Serializable {
  def name: String
  def gen[F[_] : C]: Gen[M[F[I]]]
  def run[F[_] : C]: M[F[I]] => Any
  final def forget: TCBenchmark[C, Nothing, Nothing] =
    this.asInstanceOf[TCBenchmark[C, Nothing, Nothing]]
}

abstract class ConstTCBenchmark[C[_[_]], In](benchName: String, input: Gen[In])
  extends TCBenchmark[C, Const[In, ?], Nothing] {
  override def name: String = benchName
  override def gen[F[_] : C]: Gen[In] = input
}

case class TCBenchSuite[C[_[_]]](varieties: Seq[TCBenchVariety[C, Nothing]],
                                 benchmarks: Seq[TCBenchmark[C, Nothing, Nothing]]) {
  final def forget: TCBenchSuite[Nothing] = this.asInstanceOf[TCBenchSuite[Nothing]]
}

trait TCBenchmarkRunner[Ctx] {
  def runSuite[C[_[_]]](ctx: Ctx, suite: TCBenchSuite[C]): Unit
}
