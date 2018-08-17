package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID, SymbolicHeap}
import at.forsyte.harrsh.{ExampleSIDs, TestValues}

object EntailmentAutomatonTest extends TestValues {

  def main(args: Array[String]): Unit = {

    // Entailment check: x1 -> x2 |= nel(x1, x2)
    val nel = ExampleSIDs.Nel
    val rhs = P("nel")(x1,x2)
    val lhs = SID.fromSymbolicHeap(SymbolicHeap(x1 -> x2))
    //println(s"Success: " + check(nel, rhs, lhs, expectEmpty = false))

    // Entailment check: ex. y . x1 -> y * y -> x2 |= nel(x1, x2)
    val lhs2 = SID("twoptrs",
      "List of length 2",
      Map("twoptrs" -> x1),
      // twoptrs <= emp : { a = b }
      ("oneptr", Seq.empty, SymbolicHeap(x1 -> x2)),
      // twoptrs <= ∃ y . a -> y * twoptrs(y, b)
      ("twoptrs", Seq("y"), SymbolicHeap(x1 -> y1, P("oneptr")(y1, x2)))
    )
    println(s"Success: " + check(nel, rhs, lhs2, expectEmpty = false))

  }

  def check(sid: SID, rhs: PredCall, lhs: SID, expectEmpty: Boolean): Boolean = {
    println(s"Checking ${lhs.callToStartPred} |= $rhs for SID '${sid.description}'")
    val aut = new EntailmentAutomaton(sid, rhs)
    val reachable = RefinementAlgorithms.allReachableStates(lhs, aut, reportProgress = true)
    for {
      (str, state) <- reachable
    } println(s"$str :: $state")
    true
//    val refined = RefinementAlgorithms.refineSID(lhs, aut, reportProgress = true)
//    println(refined._1)
//    expectEmpty == refined._2
  }

}
