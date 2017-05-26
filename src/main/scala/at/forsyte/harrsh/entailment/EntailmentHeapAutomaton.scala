package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.learning.{ObservationTable, TableEntry}
import at.forsyte.harrsh.heapautomata.{FVBound, HeapAutomaton}
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/6/17.
  */
class EntailmentHeapAutomaton(val numFV : Int, val obs : ObservationTable) extends HeapAutomaton with FVBound {

  val stateMap : Map[Int,TableEntry] = Map() ++ obs.entries.zipWithIndex.map(_.swap)

  override type State = Int
  override lazy val states: Set[State] = stateMap.keySet

  override def isFinal(s: State): Boolean = stateMap(s).isFinal

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    // Shrinking + entailment check(s)

    // TODO Be clever in the selection of candidate targets (restriction to right number of FVs!)

    // TODO Figure out the details. The problem is that there can be some non-determinism left if extensions only differ in pure formulas

    throw new NotImplementedError("No smart target computation for this heap automaton")
  }
}

object EntailmentHeapAutomaton {

  def fromObservationTable(numFV : Int, observationTable: ObservationTable) : EntailmentHeapAutomaton = {
    // TODO Closure of entries under permutations + null introduction
    // TODO (conditional?) introduction of emp class --> might want to see if it coincides with a non-emp class (because in a given SID there might be no symmetry at all)
    ???
  }

  def serialize(aut: EntailmentHeapAutomaton) : String = {
    val nonSinkStates = aut.obs.entries map toStateDescription
    val indexedStates = nonSinkStates.zipWithIndex.map(pair => "    " + pair._2 + " = " + pair._1)
    "EntailmentAutomaton {\n" + "  fvbound = " + aut.numFV + "\n" + "  non-sink-states = {\n" + indexedStates.mkString("\n") + "\n  }\n}"
  }

  private def toStateDescription(entry : TableEntry) : String = {
    ("{\n"
      + entry.reps.mkString("      representatives = {", ", ", "}\n")
      + entry.exts.mkString("      extensions = {", ", ", "}\n")
      + "    }")
  }

  def fromString(s : String) : EntailmentHeapAutomaton = ???

}
