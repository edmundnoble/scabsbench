package scolls

import org.scalameter.api._
import org.scalameter.picklers.IntPickler

object RangeBenchmark
  extends Bench.OfflineRegressionReport {

  implicit val intPickler = IntPickler

  val sizes = Gen.enumeration("sizes")(10, 100, 1000, 2000)

  import KaplanTarjanSteq.Steq

  @annotation.tailrec
  def steqConsRec(s: Steq[Int], i: Int): Steq[Int] =
    if (i == 0) s
    else steqConsRec(KaplanTarjanSteq.cons(i, s), i - 1)

  @annotation.tailrec
  def listConsRec(s: List[Int], i: Int): List[Int] =
    if (i == 0) s
    else listConsRec(i :: s, i - 1)

  @annotation.tailrec
  def steqSnocRec(s: Steq[Int], i: Int): Steq[Int] =
    if (i == 0) s
    else steqSnocRec(KaplanTarjanSteq.snoc(s, i), i - 1)

  @annotation.tailrec
  def listSnocRec(s: List[Int], i: Int): List[Int] =
    if (i == 0) s
    else listSnocRec(s :+ i,  i - 1)

  performance of "cons" in {
    measure method "steq" in {
      using(sizes) in {
        steqConsRec(KaplanTarjanSteq.empty[Int], _)
      }
    }
    measure method "list" in {
      using(sizes) in {
        listConsRec(Nil, _)
      }
    }
  }

  performance of "snoc" in {
    measure method "steq" in {
      using(sizes) in {
        steqSnocRec(KaplanTarjanSteq.empty[Int], _)
      }
    }
    measure method "list" in {
      using(sizes) in {
        listSnocRec(Nil, _)
      }
    }
  }


}
