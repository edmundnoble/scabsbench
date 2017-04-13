package scabs
package test
package free
package monad

import scabs.seq.Sequence
import scabs.test.seq.Tests

class TestReport extends ScalatestSuiteK[Sequence](Tests.suite)
