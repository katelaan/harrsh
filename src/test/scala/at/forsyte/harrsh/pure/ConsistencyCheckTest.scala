package at.forsyte.harrsh.pure

import at.forsyte.harrsh.Implicits
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 5/17/17.
  */
class ConsistencyCheckTest extends HarrshTableTest with Implicits {

  val IsConsistent = true
  val Inconsistent = !IsConsistent

  val inputs = Table(
    ("heap", "isConsistent"),
    ("emp", IsConsistent),
    ("x1 -> x2", IsConsistent),
    ("x1 -> x2 * x2 -> x1", IsConsistent),
    ("x1 -> x2 : {x1 = x2}", IsConsistent),
    ("x1 -> y1 * y1 -> y2 : {x1 != y1, y2 != y3, y4 = y5}", IsConsistent),
    ("x1 -> x2 : {x1 = null}", Inconsistent),
    ("x1 -> x2 * x1 -> x2", Inconsistent),
    ("x1 -> x2 * y1 -> x2 : {x1 = y1}", Inconsistent),
    ("x1 -> x2 : { x2 = x1, x2 = null}", Inconsistent),
    ("emp : { x1 = x2, x2 = x3, x3 = x4, x4 != x1}", Inconsistent),
    ("x1 -> x2 : { x1 = x2, x2 = x3, x3 = x4, x4 = null}", Inconsistent)
  )

  property("Correctness of consistency check") {

    forAll(inputs) {
      (sh: String, expectedResult: Boolean) =>
        Given(sh)
        Then("Consistency should " + (if (expectedResult) "HOLD" else "NOT HOLD"))
        ConsistencyCheck.isConsistent(sh.parse) shouldBe expectedResult
    }

  }

}
