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
    // Various ways to write some small acyclic lists
    ("x1 -> y1 : {y1 = x2, y1 != x1}", "sll-acyc.sid", true),
    ("x1 -> y1 * y1 -> x2 : {x1 != x2, y1 != x2}", "sll-acyc.sid", true),
    ("x1 -> y1 * y1 -> x2 : {x1 != y1, x1 != x2, y1 != x2}", "sll-acyc.sid", true),
    ("x1 -> y1 * y3 -> y5 : {y1 = y3, y5 = x2, y4 = y5, x1 != y1, y4 != x1, y4 != y3}", "sll-acyc.sid", true),
    // Dropping atoms of the previous formula should render entailment false, because we usually lose the acyclicity guarantee:
    ("x1 -> y1 * y3 -> y5 : {y1 = y3, y5 = x2, y4 = y5, x1 != y1, y4 != x1}", "sll-acyc.sid", false),
    ("x1 -> y1 * y3 -> y5 : {y1 = y3, y5 = x2, y4 = y5, x1 != y1, y4 != y3}", "sll-acyc.sid", false),
    ("x1 -> y1 * y3 -> y5 : {y1 = y3, y5 = x2, y4 = y5, y4 != x1, y4 != y3}", "sll-acyc.sid", true), // x1 != y1 is already entailed by y1 = y3 + allocation
    ("x1 -> y1 * y3 -> y5 : {y1 = y3, y5 = x2, x1 != y1, y4 != x1, y4 != y3}", "sll-acyc.sid", false),
    ("x1 -> y1 * y3 -> y5 : {y1 = y3, y4 = y5, x1 != y1, y4 != x1, y4 != y3}", "sll-acyc.sid", false),
    ("x1 -> y1 * y3 -> y5 : {y5 = x2, y4 = y5, x1 != y1, y4 != x1, y4 != y3}", "sll-acyc.sid", false)

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

//    val (rshString, sidString, res) = ("x1 -> y1 * y1 -> x2 : {x1 != x2, y1 != x2}", "sll-acyc.sid", true)
//    val rsh = rshString.parse
//    val sid = sidString.load()
//    val call = sid.callToStartPred
//    info("Checking " + rsh + " |= " + call + " (should be " + res + ")")
//    GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(rsh, call, sid) shouldBe res

  }

}
