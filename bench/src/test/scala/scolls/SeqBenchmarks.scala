package scolls

import org.scalameter.api._
import org.scalameter.picklers.IntPickler

import scala.annotation.tailrec

object SeqBenchmarks
  extends Bench.OfflineReport {

  import StdlibInstances._

  implicit val intPickler = IntPickler

  val constructSizes: Gen[Int] = Gen.enumeration("sizes")(10, 100, 1000, 2000)
  val destructSizes: Gen[Int] = Gen.enumeration("sizes")(100, 1000)
  val treeSizes: Gen[Int] = Gen.enumeration("treeSizes")(100, 500, 1200)
  val balancedTreeSizes: Gen[Int] = Gen.enumeration("treeSizes")(15, 20)
  val leftNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateLeftNestedTree)
  val rightNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateRightNestedTree)
  val jaggedNestedTrees: Gen[LTree[Int]] = treeSizes.map(generateJaggedTree)
  val balancedNestedTrees: Gen[LTree[Int]] = balancedTreeSizes.map(generateBalancedTree)

  def generateLeftNestedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else {
        val newLeaf = Lf(cnt)
        generate(Bin(acc.size + 1, acc, newLeaf), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def generateRightNestedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else {
        val newLeaf = Lf(cnt)
        generate(Bin(acc.size + 1, newLeaf, acc), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def generateJaggedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else if (cnt % 2 == 0) {
        generate(Bin(acc.size + 1, Lf(cnt), acc), cnt - 1)
      } else {
        generate(Bin(acc.size + 1, acc, Lf(cnt)), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def generateBalancedTree(size: Int): LTree[Int] = {
    @tailrec def generate(acc: LTree[Int], cnt: Int): LTree[Int] = {
      if (cnt == 0) {
        acc
      } else {
        generate(Bin(acc.size * 2, acc, acc), cnt - 1)
      }
    }
    generate(Lf(size), size)
  }

  def testConsSeqOnes[S[_] : Sequence]: Gen[S[Int]] = for {
    size <- destructSizes
    seq = consRecBench[S](size)
  } yield seq

  def testSnocSeqOnes[S[_] : Sequence]: Gen[S[Int]] = for {
    size <- destructSizes
    seq = snocRecBench[S](size)
  } yield seq

  import KaplanTarjanSteq.Steq

  def consRecBench[S[_]](size: Int)(implicit S: Sequence[S]): S[Int] = {
    @annotation.tailrec
    def loop(s: S[Int], cnt: Int): S[Int] = {
      if (cnt == 0) {
        s
      } else {
        loop(S.cons(cnt, s), cnt - 1)
      }
    }
    loop(S.empty[Int], size)
  }

  def snocRecBench[S[_]](size: Int)(implicit S: Sequence[S]): S[Int] = {
    @annotation.tailrec
    def loop(s: S[Int], cnt: Int): S[Int] = {
      if (cnt == 0) {
        s
      } else {
        loop(S.snoc(s, cnt), cnt - 1)
      }
    }
    loop(S.empty[Int], size)
  }

  def sumBench[S[_]](seq: S[Int])(implicit S: Sequence[S]): Int = {
    S.fold(seq)(0)(_ + _)
  }

  def concatBench[S[_], A](seq: List[S[A]])(implicit S: CSequence[S]): S[A] = {
    seq.reduceRight(S.concat)
  }

  def frontierCPS[S[_], A](tree: LTree[A])(implicit S: Sequence[S]): S[A] = {
    def helper(innerTree: LTree[A]): S[A] => S[A] = innerTree match {
      case Lf(a) => S.cons(a, _)
      case Bin(_, left, right) => helper(left) compose helper(right)
    }
    helper(tree)(S.empty)
  }

  def frontierRec[S[_], A](tree: LTree[A])(implicit S: CSequence[S]): S[A] =
    tree match {
      case Lf(a) => S.sequence.cons(a, S.sequence.empty)
      case Bin(_, left, right) => S.concat(frontierRec[S, A](left), frontierRec[S, A](right))
    }

  performance of "cons" in {
    measure method "list" in (using(constructSizes) in consRecBench[List])
    measure method "steq" in (using(constructSizes) in consRecBench[Steq])
    measure method "vec" in (using(constructSizes) in consRecBench[Vector])
    measure method "que" in (using(constructSizes) in consRecBench[OkasakiQueue])
    measure method "hque" in (using(constructSizes) in consRecBench[HQueue])
    measure method "cat" in (using(constructSizes) in consRecBench[Catenable])
  }

  performance of "snoc" in {
    measure method "list" in (using(constructSizes) in snocRecBench[List])
    measure method "steq" in (using(constructSizes) in snocRecBench[Steq])
    measure method "vec" in (using(constructSizes) in snocRecBench[Vector])
    measure method "que" in (using(constructSizes) in snocRecBench[OkasakiQueue])
    measure method "hque" in (using(constructSizes) in snocRecBench[HQueue])
    measure method "cat" in (using(constructSizes) in snocRecBench[Catenable])
  }

  performance of "summing cons-constructed seq" in {
    measure method "list" in (using(testConsSeqOnes[List]) in sumBench[List])
    measure method "steq" in (using(testConsSeqOnes[Steq]) in sumBench[Steq])
    measure method "vec" in (using(testConsSeqOnes[Vector]) in sumBench[Vector])
    measure method "que" in (using(testConsSeqOnes[OkasakiQueue]) in sumBench[OkasakiQueue])
    measure method "hque" in (using(testConsSeqOnes[HQueue]) in sumBench[HQueue])
    measure method "cat" in (using(testConsSeqOnes[Catenable]) in sumBench[Catenable])
  }

  performance of "summing snoc-constructed seq" in {
    measure method "list" in (using(testSnocSeqOnes[List]) in sumBench[List])
    measure method "steq" in (using(testSnocSeqOnes[Steq]) in sumBench[Steq])
    measure method "vec" in (using(testSnocSeqOnes[Vector]) in sumBench[Vector])
    measure method "que" in (using(testSnocSeqOnes[OkasakiQueue]) in sumBench[OkasakiQueue])
    measure method "hque" in (using(testSnocSeqOnes[HQueue]) in sumBench[HQueue])
    measure method "cat" in (using(testSnocSeqOnes[Catenable]) in sumBench[Catenable])
  }

//  performance of "concatenating cons-constructed seqs" in {
//    measure method "list" in (using(testConsSeqOnes[List]) in concatBench[List, Int])
//    measure method "vec" in (using(testConsSeqOnes[Vector]) in concatBench[Vector, Int])
//    measure method "que" in (using(testConsSeqOnes[OkasakiQueue]) in concatBench[OkasakiQueue, Int])
//    measure method "hque" in (using(testConsSeqOnes[HQueue]) in concatBench[HQueue, Int])
//    measure method "cat" in (using(testSnocSeqOnes[Catenable]) in concatBench[Catenable, Int])
//  }
//
//  performance of "concatenating snoc-constructed seqs" in {
//    measure method "list" in (using(testSnocSeqOnes[List]) in concatBench[List, Int])
//    measure method "vec" in (using(testSnocSeqOnes[Vector]) in concatBench[Vector, Int])
//    measure method "que" in (using(testSnocSeqOnes[OkasakiQueue]) in concatBench[OkasakiQueue, Int])
//    measure method "hque" in (using(testSnocSeqOnes[HQueue]) in concatBench[HQueue, Int])
//    measure method "cat" in (using(testSnocSeqOnes[Catenable]) in concatBench[Catenable, Int])
//  }

  performance of "concatenating left-nested trees" in {
    measure method "list" in (using(leftNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(leftNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(leftNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(leftNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(leftNestedTrees) in frontierRec[Catenable, Int])
  }

  performance of "concatenating right-nested trees" in {
    measure method "list" in (using(rightNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(rightNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(rightNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(rightNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(rightNestedTrees) in frontierRec[Catenable, Int])
  }

  performance of "concatenating jagged trees" in {
    measure method "list" in (using(jaggedNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(jaggedNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(jaggedNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(jaggedNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(jaggedNestedTrees) in frontierRec[Catenable, Int])
  }

  performance of "concatenating balanced trees" in {
    measure method "list" in (using(balancedNestedTrees) in frontierRec[List, Int])
    measure method "vec" in (using(balancedNestedTrees) in frontierRec[Vector, Int])
    measure method "que" in (using(balancedNestedTrees) in frontierRec[OkasakiQueue, Int])
    measure method "hque" in (using(balancedNestedTrees) in frontierRec[HQueue, Int])
    measure method "cat" in (using(balancedNestedTrees) in frontierRec[Catenable, Int])
  }

  performance of "functionally concatenating left-nested trees" in {
    measure method "list" in (using(leftNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(leftNestedTrees) in frontierCPS[Vector, Int])
    measure method "que" in (using(leftNestedTrees) in frontierCPS[OkasakiQueue, Int])
    measure method "hque" in (using(leftNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(leftNestedTrees) in frontierCPS[Catenable, Int])
  }

  performance of "functionally concatenating right-nested trees" in {
    measure method "list" in (using(rightNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(rightNestedTrees) in frontierCPS[Vector, Int])
    measure method "que" in (using(rightNestedTrees) in frontierCPS[OkasakiQueue, Int])
    measure method "hque" in (using(rightNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(rightNestedTrees) in frontierCPS[Catenable, Int])
  }

  performance of "functionally concatenating jagged trees" in {
    measure method "list" in (using(jaggedNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(jaggedNestedTrees) in frontierCPS[Vector, Int])
    measure method "vec" in (using(jaggedNestedTrees) in frontierCPS[Vector, Int])
    measure method "hque" in (using(jaggedNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(jaggedNestedTrees) in frontierCPS[Catenable, Int])
  }

  performance of "functionally concatenating balanced trees" in {
    measure method "list" in (using(balancedNestedTrees) in frontierCPS[List, Int])
    measure method "vec" in (using(balancedNestedTrees) in frontierCPS[Vector, Int])
    measure method "que" in (using(balancedNestedTrees) in frontierCPS[OkasakiQueue, Int])
    measure method "hque" in (using(balancedNestedTrees) in frontierCPS[HQueue, Int])
    measure method "cat" in (using(balancedNestedTrees) in frontierCPS[Catenable, Int])
  }


}
