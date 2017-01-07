package scabs

import scabs.Util.Lub1

package object colls {
  type TraitBenchmark[C[_], M[_], I] = TCBenchmark[Lub1[?, C], M, I]
  type ConstTraitBenchmark[C[_], I] = ConstTCBenchmark[Lub1[?, C], I]
}
