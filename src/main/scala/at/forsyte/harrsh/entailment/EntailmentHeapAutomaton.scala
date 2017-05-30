package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.learning.{ObservationTable, TableEntry}
import at.forsyte.harrsh.heapautomata.{FVBound, HeapAutomaton, TargetComputation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConsistencyCheck
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

    // We need a single sink state for those heaps that are consistent but cannot be extended to unfoldings of the RHS
    val sinkState : State = stateToEntryMap.size + 1
    // Inconsistent heaps always make the entailment true, so we have a distinguished inconsistent state
    val inconsistentState : State = sinkState + 1

    logger.debug("Automaton state space:\n" + stateToEntryMap.toSeq.sortBy(_._1).map{ pair => (pair._1, pair._2, isFinal(pair._1))}.mkString("\n"))

    def setOfStates : Set[State] = stateToEntryMap.keySet + sinkState + inconsistentState

    // TODO Guarantee that we take the spatially smallest representative. Do we even want to keep more than one rep?
    def rep(s : State) : SymbolicHeap = {
      assert(!isSink(s) && !isInconsistent(s))
      stateToEntryMap(s).reps.head
    }

    def isFinal(s : State) : Boolean = negate != (isInconsistent(s) || (!isSink(s) && stateToEntryMap(s).isFinal))

    def stateOfHeap(sh : SymbolicHeap) : State = {

      if (!ConsistencyCheck.isConsistent(sh)) {
        logger.debug("Heap " + sh + " is inconsistent => Go to accepting sink state " + inconsistentState)
        inconsistentState
      } else {
        // Try to match the heap against the equivalence classes of the SID
        val matches = obs.getAllMatchingEntries(sh)
        if (matches.isEmpty) {
          // No match => Not extensible to SID unfolding => go to sink
          logger.debug("No match for " + sh + " => Go to rejecting sink state " + sinkState)
          sinkState
        } else {
          // TODO Do we have to support "real" nondeterminism? (Currently we'll have an exception in that case.)
          //val strongest = TableEntry.entryWithWeakestExtensions(matches)
          val strongest = TableEntry.entryWithMinimalExtensionSet(matches)
          entryToStateMap(strongest)
        }
      }
    }

    def isSink(s : State) : Boolean = s == sinkState
    def isInconsistent(s : State) : Boolean = s == inconsistentState
  }

  override lazy val states: Set[State] = States.setOfStates
  override def isFinal(s: State): Boolean = States.isFinal(s)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    if (src.exists(States.isSink)) {
      // The sink state propagates
      logger.debug("Children " + src.mkString(", ") + " contain sink => target for " + lab + " is sink")
      Set(States.sinkState)
    } else {
      val shrunk = lab.replaceCalls(src map States.rep)
      logger.debug("Children " + src.mkString(", ") + " => Shrink " + lab + " to " + shrunk)
      val res = States.stateOfHeap(shrunk)
      logger.debug("Target state: " + res)
      Set(res)
    }
  }
}

object EntailmentHeapAutomaton extends HarrshLogging {

  def fromObservationTable(numFV : Int, observationTable: ObservationTable) : EntailmentHeapAutomaton = EntailmentHeapAutomaton(numFV, observationTable, negate = false)

  def decideEntailment(sid : SID, ha : EntailmentHeapAutomaton, timeout : Duration, verbose : Boolean, reportProgress : Boolean) : Boolean = {
    // The entailment is true iff the negated automaton can never reach a final state iff the intersection with the negated automaton is empty
    logger.debug("Will check refinment of " + sid + " with entailment automaton")
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
