import scabs.Util.Lub1

package object scabs {
  type Lub1C[C[_]] = { type l[A[_]] = Lub1[A, C] }
  type TraitBenchmark[C[_], M[_], I] = TCBenchmark[Lub1C[C]#l, M, I]
  type ConstTraitBenchmark[C[_], I] = ConstTCBenchmark[Lub1C[C]#l, I]
}
