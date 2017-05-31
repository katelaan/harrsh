package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.learning.{ObservationTable, TableEntry}
import at.forsyte.harrsh.heapautomata.{FVBound, HeapAutomaton, TargetComputation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConsistencyCheck
import at.forsyte.harrsh.refinement.DecisionProcedures
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

import scala.concurrent.duration.Duration

/**
  * Created by jens on 3/6/17.
  */
case class EntailmentHeapAutomaton(numFV : Int, obs : ObservationTable, negate : Boolean) extends HeapAutomaton with TargetComputation with FVBound with HarrshLogging {

  case class StateDesc(stateIndex : Int, requiredContext : Set[Var], forbiddenContext : Set[Var])

  override type State = StateDesc

  private object States {

    private val stateToEntryMap: Map[Int, TableEntry] = Map() ++ obs.entries.zipWithIndex.map(_.swap)
    // TODO For large automata, keeping two maps wastes quite a bit of memory
    private val entryToStateMap: Map[TableEntry, Int] = Map() ++ obs.entries.zipWithIndex

    def untaggedStateFromIndex(stateIndex : Int) : State = StateDesc(stateIndex, Set.empty, Set.empty)

    // We need a single sink state for those heaps that are consistent but cannot be extended to unfoldings of the RHS
    val sinkStateIndex : Int = stateToEntryMap.size + 1
    // Inconsistent heaps always make the entailment true, so we have a distinguished inconsistent state
    val inconsistentStateIndex : Int = sinkStateIndex + 1

    logger.debug("Automaton state space:\n" + stateToEntryMap.toSeq.sortBy(_._1).map{ pair => (pair._1, pair._2, isFinal(untaggedStateFromIndex(pair._1)))}.mkString("\n"))

    def stateIndices : Set[Int] = stateToEntryMap.keySet + sinkStateIndex + inconsistentStateIndex

    // TODO Guarantee that we take the spatially smallest representative. Do we even want to keep more than one rep?
    def rep(s : State) : SymbolicHeap = {
      assert(!isSink(s) && !isInconsistent(s))
      stateToEntryMap(s.stateIndex).reps.head
    }

    def isFinal(s : State) : Boolean = if (s.requiredContext.nonEmpty) {
      // There are external constraints left => Reject
      false
    } else {
      // If the goal is to prove entailment, accept inconsistent heaps and those that correspond to an accepting table entry;
      // If the goal is to prove non-entailment, negate
      negate != (isInconsistent(s) || (!isSink(s) && stateToEntryMap(s.stateIndex).isFinal))
    }

    def statesOfHeap(sh : SymbolicHeap) : Set[State] = {

      val stateIndex = stateWithoutExternalAssumptions(sh)
      // Close under possible external assumptions
      Set(untaggedStateFromIndex(stateIndex)) ++ targetStatesWithExternalAssumptions(sh)
    }

    private def stateWithoutExternalAssumptions(sh : SymbolicHeap) : Int = {
      if (!ConsistencyCheck.isConsistent(sh)) {
        logger.debug("Heap " + sh + " is inconsistent => Go to accepting sink state " + inconsistentStateIndex)
        inconsistentStateIndex
      } else {
        // Try to match the heap against the equivalence classes of the SID
        val matches = obs.getAllMatchingEntries(sh)
        if (matches.isEmpty) {
          // No match => Not extensible to SID unfolding => go to sink
          logger.debug("No match for " + sh + " => Check if we get match under external assumptions" + sinkStateIndex)
          // FIXME Check external assumptions
          sinkStateIndex
        } else {
          // TODO Do we have to support "real" nondeterminism w.r.t. state indeices as well? (Currently we'll have an exception in that case.)
          //val strongest = TableEntry.entryWithWeakestExtensions(matches)
          val strongest = TableEntry.entryWithMinimalExtensionSet(matches)
          entryToStateMap(strongest)
        }
      }
    }

    private def targetStatesWithExternalAssumptions(sh : SymbolicHeap) : Set[State] = {
      val candidates = taggingCandidates(sh)
      logger.debug("Considering tagging options " + candidates.map(_._1).mkString(", ") + " for " + sh)
      for {
        (tag, atoms) <- candidates
        extendedSh = sh.copy(pure = sh.pure ++ atoms)
      } yield StateDesc(stateWithoutExternalAssumptions(extendedSh), tag, Set.empty)
    }

    def isSink(s : State) : Boolean = s.stateIndex == sinkStateIndex
    def isInconsistent(s : State) : Boolean = s.stateIndex == inconsistentStateIndex
  }

  override lazy val states: Set[State] = for {
    index <- States.stateIndices
    required <- Var.mkSetOfAllVars(1 to numFV).subsets()
    forbidden <- Var.mkSetOfAllVars(1 to numFV).subsets()
  } yield StateDesc(index, required, forbidden)

  override def isFinal(s: State): Boolean = States.isFinal(s)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    if (src.exists(States.isSink)) {
      // The sink state propagates
      logger.debug("Children " + src.mkString(", ") + " contain sink => target for " + lab + " is sink")
      Set(States.untaggedStateFromIndex(States.sinkStateIndex))
    } else {
      val shrunk = lab.replaceCalls(src map States.rep)
      logger.debug("Children " + src.mkString(", ") + " => Shrink " + lab + " to " + shrunk)

      val allocRequirements = renameExternalRequirements(src.map(_.requiredContext) zip lab.predCalls)
      val remainingRequirements = unmetRequirements(shrunk, allocRequirements)
      if (remainingRequirements.exists(_.isBound)) {
        logger.debug("Remaining allocation requirements " + remainingRequirements + " cannot be met later since they contain a bound var => discard state")
        Set.empty
      } else {
        val shrunkHeapWithRequirements = if (remainingRequirements.isEmpty) shrunk
        else {
          val constraints = externalConstraints(shrunk, remainingRequirements)
          logger.debug("External requirements " + allocRequirements + "; not met by " + shrunk + ": " + remainingRequirements + " => additional constraint " + constraints)
          shrunk.copy(pure = shrunk.pure ++ constraints)
        }
        val res = States.statesOfHeap(shrunkHeapWithRequirements).map(s => s.copy(requiredContext = remainingRequirements ++ s.requiredContext))
        logger.debug("Target states: " + res)
        res
      }
    }
  }

  private def nonalloced(sh : SymbolicHeap): Set[Var] = {
    Var.mkSetOfAllVars(1 to sh.numFV) -- sh.alloc
  }

  private def externalConstraints(sh : SymbolicHeap, externalAllocAssumption : Set[Var]) : Set[PureAtom] = {
    (for {
      allocedVar <- sh.alloc
      externalVar <- externalAllocAssumption
    } yield PtrNEq(allocedVar, externalVar)).toSet
  }

  private def taggingCandidates(sh : SymbolicHeap) : Set[(Set[Var], Set[PureAtom])] = {
    (for {
      externalAllocationOption <- nonalloced(sh).subsets()
      if externalAllocationOption.nonEmpty
      constraints = externalConstraints(sh, externalAllocationOption)
    } yield (externalAllocationOption, constraints)).toSet
  }

  private def unmetRequirements(sh : SymbolicHeap, allocRequirements : Set[Var]) : Set[Var] = {
    allocRequirements diff sh.alloc
  }

  private def renameExternalRequirements(requirementMatching : Seq[(Set[Var], PredCall)]) : Set[Var] = {
    (requirementMatching flatMap {
      case (vars, call@PredCall(_, args)) =>
        logger.debug("Renaming " + vars + " according to " + call)
        vars map {
          case v@Var(i) =>
            assert(v.isFree)
            args(i-1).getVarOrZero
      }
    }).toSet
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
