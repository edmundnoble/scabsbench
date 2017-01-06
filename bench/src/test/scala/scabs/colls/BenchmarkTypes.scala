package scabs.colls

import org.scalameter.api._
import scabs.Util.Const

final case class TCBenchVariety[S[_[_]], M[_]](name: String)
                                              (implicit val instance: S[M]) {
  def forget: TCBenchVariety[S, Nothing] =
    this.asInstanceOf[TCBenchVariety[S, Nothing]]
}

abstract class TCBenchmark[S[_[_]], M[_], I] extends Serializable {
  def name: String
  def gen[F[_] : S]: Gen[M[F[I]]]
  def run[F[_] : S]: M[F[I]] => Any
  final def forget: TCBenchmark[S, Nothing, Nothing] =
    this.asInstanceOf[TCBenchmark[S, Nothing, Nothing]]
}

abstract class ConstTCBenchmark[S[_[_]], In](benchName: String, input: Gen[In])
  extends TCBenchmark[S, Const[In, ?], Nothing] {
  override def name: String = benchName
  override def gen[F[_] : S]: Gen[In] = input
}

abstract class TCBenchSuite[S[_[_]]] {
  def varieties: Seq[TCBenchVariety[S, Nothing]]
  def benchmarks: Seq[TCBenchmark[S, Nothing, Nothing]]
  final def forget: TCBenchSuite[Nothing] = this.asInstanceOf[TCBenchSuite[Nothing]]
}

trait TCBenchmarkRunner[Ctx] {
  def runSuite[S[_[_]]](ctx: Ctx, suite: TCBenchSuite[S]): Unit
}

object TCScalameterRunner extends TCBenchmarkRunner[Bench[Double]] {

  private def runWithVariety[S[_[_]], M[_], R[_], I](bench: Bench[Double], benchmark: TCBenchmark[S, R, I], variety: TCBenchVariety[S, M]): Unit = {
    implicit val instance = variety.instance
    bench.measure method variety.name in {
      bench.using(benchmark.gen[M]) in benchmark.run[M]
    }
  }

  override def runSuite[S[_[_]]](ctx: Bench[Double], suite: TCBenchSuite[S]): Unit = {
    suite.benchmarks.foreach { b =>
      ctx.performance of b.name in {
        suite.varieties.foreach(runWithVariety[S, Nothing, Nothing, Nothing](ctx, b, _))
      }
    }
  }

}
