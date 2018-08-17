package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.HeapAutomaton
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
    val reachable: Set[(String, EntailmentAutomaton.State)] = RefinementAlgorithms.allReachableStates(lhs, aut, reportProgress = true)
    for {
      (str, state) <- reachable
    } println(s"$str :: $state")
    val isFinal = (s: EntailmentAutomaton.State) => aut.isFinal(s)
    println(serializeReach(reachable, isFinal))
    true
//    val refined = RefinementAlgorithms.refineSID(lhs, aut, reportProgress = true)
//    println(refined._1)
//    expectEmpty == refined._2
  }

  def indent(s : String) = "  " + s

  def serializeReach(states: Set[(String, EntailmentAutomaton.State)], isFinal: EntailmentAutomaton.State => Boolean): String = {
    val statesByPred: Map[String, Set[EntailmentAutomaton.State]] = states.groupBy(_._1).mapValues(pairs => pairs.map(_._2))
    val lines = Stream("RESULT {") ++ statesByPred.toStream.flatMap(pair => serializePred(pair._1, pair._2, isFinal)).map(indent) ++ Stream("}")
    lines.mkString("\n")
  }

  def serializePred(pred: String, states: Set[EntailmentAutomaton.State], isFinal: EntailmentAutomaton.State => Boolean): Stream[String] = {
    Stream(s"PRED $pred {") ++ states.toStream.flatMap(s => serializeState(s, isFinal)).map(indent) ++ Stream("}")
  }

  def serializeState(state: EntailmentAutomaton.State, isFinal: EntailmentAutomaton.State => Boolean): Stream[String] = {
    (Stream("STATE {",
      s"  PARAMS: ${state.orderedParams.mkString(", ")}")
      ++ Some("  FINAL").filter(_ => isFinal(state))
    ++ state.ets.toStream.flatMap(serializeET).map(indent) ++ Stream("}"))
  }

  def serializeET(et: ExtensionType): Stream[String] = {
    et.toString.lines.toStream
  }

}
