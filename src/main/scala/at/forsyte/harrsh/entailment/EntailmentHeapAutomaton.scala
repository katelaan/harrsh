package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.learning.{ObservationTable, TableEntry}
import at.forsyte.harrsh.heapautomata.{FVBound, HeapAutomaton, TargetComputation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.refinement.DecisionProcedures
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

import scala.concurrent.duration.Duration

/**
  * Created by jens on 3/6/17.
  */
case class EntailmentHeapAutomaton(numFV : Int, obs : ObservationTable, negate : Boolean) extends HeapAutomaton with TargetComputation with FVBound with HarrshLogging {

  override type State = Int

  private object States {
    private val stateToEntryMap: Map[Int, TableEntry] = Map() ++ obs.entries.zipWithIndex.map(_.swap)
    // TODO For large automata, keeping two maps wastes quite a bit of memory
    private val entryToStateMap: Map[TableEntry, Int] = Map() ++ obs.entries.zipWithIndex

    logger.debug("Automaton state space:\n" + stateToEntryMap.toSeq.sortBy(_._1).map{ pair => (pair._1, pair._2, isFinal(pair._1))}.mkString("\n"))

    def setOfStates : Set[State] = stateToEntryMap.keySet

    // TODO: Guarantee that we take the spatially smallest representative. Do we even want to keep more than one rep?
    def rep(s : State) : SymbolicHeap = stateToEntryMap(s).reps.head

    def isFinal(s : State) : Boolean = negate != stateToEntryMap(s).isFinal

    def stateOfHeap(sh : SymbolicHeap) : State = {
      // TODO Be clever in the selection of candidate targets (restriction to right number of FVs!)
      val matches = obs.getAllMatchingEntries(sh)

      // TODO Do we have to support "real" nondeterminism? (Currently we'll have an exception in that case.)
      val strongest = TableEntry.weakestEntry(matches)
      entryToStateMap(strongest)
    }
  }

  override lazy val states: Set[State] = States.setOfStates
  override def isFinal(s: State): Boolean = States.isFinal(s)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    val shrunk = lab.replaceCalls(src map States.rep)
    logger.debug("Children " + src.mkString(", ") + " => Shrink " + lab + " to " + shrunk)
    val res = States.stateOfHeap(shrunk)
    logger.debug("Target state: " + res)
    Set(res)
  }
}

object EntailmentHeapAutomaton extends HarrshLogging {

  def fromObservationTable(numFV : Int, observationTable: ObservationTable) : EntailmentHeapAutomaton = EntailmentHeapAutomaton(numFV, observationTable, negate = false)

  def decideEntailment(sid : SID, ha : EntailmentHeapAutomaton, timeout : Duration, verbose : Boolean, reportProgress : Boolean) : Boolean = {
    // The entailment is true iff the negated automaton can never reach a final state iff the intersection with the negated automaton is empty
    val negatedHA = ha.copy(negate = true)
    val res : DecisionProcedures.AnalysisResult = DecisionProcedures.decideInstance(sid, negatedHA, timeout, verbose = verbose, reportProgress = reportProgress)
    logger.debug(res.toString)
    res.isEmpty
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
