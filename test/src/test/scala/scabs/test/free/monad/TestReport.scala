package scabs
package test
package free
package monad

import cats.implicits._

class TestReport extends ScalatestSuiteK(new Tests[Option].suite)
