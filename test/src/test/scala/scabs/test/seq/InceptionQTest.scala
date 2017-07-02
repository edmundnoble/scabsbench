package scabs.test.seq

import org.scalatest._
import scabs.seq.{InceptionQ, Sequence}
import InceptionQ._
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Gen, Prop}
import scabs.test.seq.Tests.StackOpsTest

/**
  *
  *
  * @author Matthew Pocock
  */
class InceptionQTest extends WordSpec with PropertyChecks with Matchers with AppendedClues {

  "An InceptionQ" when {
    "empty" should {
      "be empty" in {
        val ei = InceptionQ.empty[Int]
        ei.isEmpty should be(true)
      }

      "reuse the same instance" in {
        val ei = InceptionQ.empty[Int]
        val es = InceptionQ.empty[String]
        ei should be theSameInstanceAs es
      }

      "append to itself" should {
        val sum = InceptionQ.empty[Int] ++ InceptionQ.empty[Int]

        "be empty" in {
          sum.isEmpty should be(true)
        }

        "be the same instance" in {
          sum should be theSameInstanceAs InceptionQ.empty[Int]
        }
      }
    }

    "any queue" should {
      implicit val genOps: Gen[StackOps[Int]] = StackOps.genOps[Int]

      "cons uncons" in {
        forAll(genOps)(ops => {
          val queue = ops.replay[InceptionQ]

          val cQueue = 42 +: queue
          val Some((h, dQ)) = cQueue.uncons

          cQueue.size should be(queue.size + 1)
          dQ.size should be(queue.size)
          h should be(42)
        })
      }

      "snoc unsnoc" in {
        forAll(genOps)(ops => {
          val queue = ops.replay[InceptionQ]

          val sQueue = queue :+ 42
          val Some((dQ, h)) = sQueue.unsnoc

          sQueue.size should be(queue.size + 1)
          dQ.size should be(queue.size)
          h should be(42)
        })
      }

      "uncons" should {
        "to a queue with one fewer elements" in {
          unconsDecreasesLengthByOne[InceptionQ]
        }
      }

      def unconsDecreasesLengthByOne[S[_]](implicit S: Sequence[S]): Assertion = {
        forAll(genOps)((ops: StackOps[Int]) => {
          import scabs.seq.StdlibInstances.listSequenceInstance

          val asList = ops.replay[List]
          val queue = ops.replay[S]

          var fc: S[Int] = null
          var pc = queue
          var uc = S.uncons(queue)
          var as: List[Assertion] = Nil
          for {
            (el, i) <- asList.zipWithIndex
          } {
            uc match {
              case Some((h, tl)) => {
                as ::= equal(h, el)
                as ::= equal(S.lengthSeq(tl), asList.size - i - 1)
              }
                fc = pc
                pc = tl
                uc = S.uncons(tl)
              case None =>
            }
          }
          and(as: _*)
        })

        new StackOpsTest("") {
          override def runTest[F[_] : Sequence](ops: StackOps[Int]): scabs.Assertion = unsnocDecreasesLengthByOne[S]
        }

        def unsnocDecreasesLengthByOne[S[_]](ops: StackOps[Int])(implicit S: Sequence[S]): scabs.Assertion = {
              import scabs.seq.StdlibInstances.listSequenceInstance

              val asList = ops.replay[List].reverse
              val queue = ops.replay[S]

              var fs: S[Int] = null
              var ps = queue
              var us = S.unsnoc(queue)
              var as: List[Assertion] = Nil
              for {
                (el, i) <- asList.zipWithIndex
              } {
                us match {
                  case Some((tl, h)) => {
                    as ::= equal(h, el)
                    as ::= equal(S.lengthSeq(tl), asList.size - i - 1)
                  }
                    fs = ps
                    ps = tl
                    us = S.unsnoc(tl)
                  case _ =>
                }
              }
              and(as: _*)
            }

            "unsnoc" should {
              "to a queue with one fewer elements" in {
                unsnocDecreasesLengthByOne[InceptionQ]
              }
            }
          }
        }
      }

    }
