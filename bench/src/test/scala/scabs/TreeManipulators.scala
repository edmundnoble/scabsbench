package scabs

import scabs.seq.{Bin, LTree, Lf}

import scala.annotation.tailrec

object TreeManipulators {

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


}
