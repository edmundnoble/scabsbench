package scabs.colls

object Hierarchy extends LowPri

trait LowPri {
  implicit def cSeqToSeq[S[_]](implicit S: CSequence[S]): Sequence[S] = S.sequence
}
