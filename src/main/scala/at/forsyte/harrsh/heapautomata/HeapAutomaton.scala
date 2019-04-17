package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 10/15/16.
  */
trait HeapAutomaton extends HarrshLogging {

  type State

  val description : String = "HA"

  def isFinal(s : State) :  Boolean

  def isNonSink(s : State) : Boolean = true

  /**
    * Evaluates the transition function on the given src, trg, and SH; only meant for evaluating single transitions;
    * to iterate over defined transitions for a fixed SH, use getDefinedTransitions
    */
  def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
    val targets = getTargetsFor(src, lab)
    val res = targets.contains(trg)
    logger.debug("Transition " + src.mkString(", ") + " --[" + lab + "]--> " + trg + " : " + res)
    res
  }

  /**
    * Direct target computation
    */
  def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State]

  def getTargetsAndLocalStatesFor(src : Seq[State], lab : SymbolicHeap) : (Option[State], Option[Seq[State]], Set[State]) = {
    (None, None, getTargetsFor(src, lab))
  }

  def getTransitionsFor(src : Seq[State], lab : SymbolicHeap, head: String) : Set[HeapAutomaton.Transition[State]] = {
    val (localState, instantiatedSourceStates, targets) = getTargetsAndLocalStatesFor(src, lab)
    for {
      target <- targets
    } yield HeapAutomaton.Transition(instantiatedSourceStates.getOrElse(src), lab, localState, head, target)
  }

  def implementsPartialTargets: Boolean = false

  /**
    * Replace the first src.length calls by src, the remainder by an empty heap, followed by target computation.
    */
  def getPartialTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = throw new NotImplementedError("No partial target computation for this heap automaton")

  override def toString = description

}

object HeapAutomaton {

  case class Transition[State](srcStates: Seq[State], body: SymbolicHeap, localState: Option[State], headPredicate: String, headState: State)

}
