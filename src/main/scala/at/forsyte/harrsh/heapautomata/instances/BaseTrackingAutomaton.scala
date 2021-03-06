package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.{Kernelizable, TrackingInfo}
import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState, TaggedTargetComputation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityUtils
import at.forsyte.harrsh.refinement.AutomatonTask
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PureAtom, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 10/18/16.
  */
abstract class BaseTrackingAutomaton extends HeapAutomaton with InconsistentState with TaggedTargetComputation[TrackingInfo] {

  override type State = TrackingInfo

  override val description : String = "TRACK-BASE"

  override def inconsistentState(fvs: Seq[FreeVar]): State = TrackingInfo.inconsistentTrackingInfo(fvs)

  override def getTargetsWithTags(src : Seq[State], lab : SymbolicHeap) : Set[(State,TrackingInfo)] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.identsOfCalledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.identsOfCalledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val consistencyCheckedState = compressAndPropagateTracking(src, lab)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.projectionToFreeVars

    if (logger.underlying.isDebugEnabled && consistencyCheckedState != trg)
      logger.debug("After dropping bound variables: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set((trg,consistencyCheckedState))
  }

  private def compressAndPropagateTracking(src : Seq[TrackingInfo], lab : SymbolicHeap) : TrackingInfo = {
    val compressed = Kernelizable.compressByKernelization(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)

    val stateWithClosure = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("State for compressed SH: " + stateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise return state as is
    if (stateWithClosure.isConsistent) stateWithClosure else inconsistentState(lab.freeVars)
  }

  override def implementsPartialTargets: Boolean = true

  // TODO: Reduce code duplication
  override def getPartialTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug("Computing possible partial targets " + src.mkString(", ") + " --[" + lab + "]--> ???")

    // Perform compression + subsequent equality/allocation propagation
    val consistencyCheckedState = compressAndPropagatePartialTracking(src, lab)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.projectionToFreeVars

    if (logger.underlying.isDebugEnabled && consistencyCheckedState != trg)
      logger.debug("After dropping bound variables: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set(trg)
  }

  // TODO: Reduce code duplication
  private def compressAndPropagatePartialTracking(src : Seq[TrackingInfo], lab : SymbolicHeap) : TrackingInfo = {
    val compressed = Kernelizable.compressByPartialKernelization(lab, src)
    logger.debug("Partially compressed " + lab + " into " + compressed)

    val stateWithClosure = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("State for compressed SH: " + stateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise return state as is
    if (stateWithClosure.isConsistent) stateWithClosure else inconsistentState(lab.freeVars)
  }

}

object BaseTrackingAutomaton extends HarrshLogging {

  def defaultTrackingAutomaton : BaseTrackingAutomaton = new BaseTrackingAutomaton {
    override def isFinal(s: TrackingInfo): Boolean = throw new IllegalStateException("Call to default implementation -- should have been overridden")
  }

  // TODO In all the following classes, we should probably check against the closure of pure instead

  /**
    * Tracking automaton for the given number of free variables, whose final state is specified by alloc and pure.
    */
  class TrackingAutomatonWithSingleFinalState(alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) extends BaseTrackingAutomaton {

    override val description = AutomatonTask.keywords.reltrack + "(" + alloc.mkString(",") + "; " + pure.mkString(",") + ")"

    override def isFinal(s: TrackingInfo) = (s.pure == pure && s.alloc == alloc) != negate

  }

  /**
    * Tracking automaton which checks for subset inclusion rather than equality with parameters
    */
  class SubsetTrackingAutomaton(alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) extends BaseTrackingAutomaton {

    override val description = AutomatonTask.keywords.track + "(" + alloc.mkString(",") + "; " + pure.mkString(",") + ")"

    override def isFinal(s: TrackingInfo) = (pure.subsetOf(s.pure) && alloc.subsetOf(s.alloc)) != negate

  }

  /**
    * Tracking automaton whose target states are defined only by (minimum) allocation (not by pure formulas)
    */
  class AllocationTrackingAutomaton(alloc : Set[Var], negate : Boolean = false) extends BaseTrackingAutomaton {

    override val description = AutomatonTask.keywords.alloc + "(" + alloc.mkString(",") + ")"

    override def isFinal(s: TrackingInfo) = (alloc subsetOf s.alloc) != negate

  }

  /**
    * Tracking automaton whose target states are defined only by (minimum) pure formulas (not by allocation)
    */
  class PureTrackingAutomaton(pure : Set[PureAtom], negate : Boolean = false) extends BaseTrackingAutomaton {

    override val description = AutomatonTask.keywords.pure + "(" + pure.mkString(",") + ")"

    override def isFinal(s: TrackingInfo) = (pure subsetOf s.pure) != negate

  }

}
