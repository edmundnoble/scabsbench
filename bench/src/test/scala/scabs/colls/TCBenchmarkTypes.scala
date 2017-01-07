package scabs.colls

import org.scalameter.api._
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

object TCScalameterRunner extends TCBenchmarkRunner[Bench[Double]] {

  private def runWithVariety[C[_[_]], M[_], R[_], I](bench: Bench[Double], benchmark: TCBenchmark[C, R, I], variety: TCBenchVariety[C, M]): Unit = {
    implicit val instance = variety.instance
    bench.measure method variety.name in {
      bench.using(benchmark.gen[M]) in benchmark.run[M]
    }
  }

  override def runSuite[C[_[_]]](ctx: Bench[Double], suite: TCBenchSuite[C]): Unit = {
    suite.benchmarks.foreach { b =>
      ctx.performance of b.name in {
        suite.varieties.foreach(runWithVariety[C, Nothing, Nothing, Nothing](ctx, b, _))
      }
    }
  }

}
