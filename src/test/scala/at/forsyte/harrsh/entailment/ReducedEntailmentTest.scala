package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.test.HarrshTableTest

import at.forsyte.harrsh.Implicits._

/**
  * Created by jkatelaa on 4/11/17.
  */
class ReducedEntailmentTest extends HarrshTableTest {

  val testVals = Table(
    ("rsh", "sid", "expected result"),
    ("emp", "sll.sid", false),
    ("emp : {x1 = x2}", "sll.sid", true),
    ("emp", "sll-acyc.sid", false),
    ("emp : {x1 = x2}", "sll-acyc.sid", true),
    ("x1 -> x2 : {x1 != x2}", "sll-acyc.sid", true),
    ("x1 -> x2 : {x1 = x2}", "sll-acyc.sid", false),
    ("x1 -> x2", "sll-acyc.sid", false),
    ("x1 -> y1 : {y1 = x2, y1 != x1}", "sll-acyc.sid", true)//,
    //("x1 -> y1 * y1 -> x2 : {x1 != x2, y1 != x2}", "sll-acyc.sid", true)
    //("x1 -> y1 * y1 -> x2 : {x1 != y1, x1 != x2, y1 != x2}", "sll-acyc.sid", true),
    //("x1 -> y1 * y3 -> y5 : {y1 = y3, y5 = x2, y4 = y5, x1 != y1, y4 != x1, y4 != y3}", "sll-acyc.sid", true)
  )

  property("Correctness of reduced entailment checks") {

    forAll(testVals) {
      (rshString, sidString, res) =>
        val rsh = rshString.parse
        val sid = sidString.load()
        val call = sid.callToStartPred
        info("Checking " + rsh + " |= " + call + " (should be " + res + ")")
        GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(rsh, call, sid) shouldBe res
    }

  }

}
