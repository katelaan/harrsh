package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap
import at.forsyte.harrsh.util.Combinators
import sun.reflect.generics.reflectiveObjects.NotImplementedException

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

  def implementsPartialTargets: Boolean = false

  /**
    * Replace the first src.length calls by src, the remainder by an empty heap, followed by target computation.
    */
  def getPartialTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = throw new NotImplementedError("No partial target computation for this heap automaton")

  override def toString = description

}
