import scabs.Util.Lub1

package object scabs {
  type Lub1C[C[_]] = { type l[A[_]] = Lub1[A, C] }
  type TraitBenchmark[C[_], M[_], I] = Benchmark[Lub1C[C]#l, M, I]
  type ConstTraitBenchmark[C[_], I] = ConstantInputBenchmark[Lub1C[C]#l, I]
}
