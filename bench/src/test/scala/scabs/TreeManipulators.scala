package scabs

import scabs.seq.{Bin, LTree, Lf}

import scala.annotation.tailrec

object TreeManipulators {

  def generateLeftNestedTree[A](filler: A, size: Int): LTree[A] = {
    @tailrec def generate(acc: LTree[A], cnt: Int): LTree[A] = {
      if (cnt == 0) {
        acc
      } else {
        val newLeaf = Lf(filler)
        generate(Bin(acc.size + 1, acc, newLeaf), cnt - 1)
      }
    }
    generate(Lf(filler), size)
  }

  def generateRightNestedTree[A](filler: A, size: Int): LTree[A] = {
    @tailrec def generate(acc: LTree[A], cnt: Int): LTree[A] = {
      if (cnt == 0) {
        acc
      } else {
        val newLeaf = Lf(filler)
        generate(Bin(acc.size + 1, newLeaf, acc), cnt - 1)
      }
    }
    generate(Lf(filler), size)
  }

  def generateJaggedTree[A](filler: A, size: Int): LTree[A] = {
    @tailrec def generate(acc: LTree[A], cnt: Int): LTree[A] = {
      if (cnt == 0) {
        acc
      } else if (cnt % 2 == 0) {
        generate(Bin(acc.size + 1, Lf(filler), acc), cnt - 1)
      } else {
        generate(Bin(acc.size + 1, acc, Lf(filler)), cnt - 1)
      }
    }
    generate(Lf(filler), size)
  }

  def generateBalancedTree[A](filler: A, size: Int): LTree[A] = {
    @tailrec def generate(acc: LTree[A], cnt: Int): LTree[A] = {
      if (cnt == 0) {
        acc
      } else {
        generate(Bin(acc.size * 2, acc, acc), cnt - 1)
      }
    }
    generate(Lf(filler), size)
  }


}
