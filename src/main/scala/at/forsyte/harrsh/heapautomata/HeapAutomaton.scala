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

  val states : Set[State]

  def isFinal(s : State) :  Boolean

  /**
    * Is the given SH in the alphabet of this HA?
    */
  def doesAlphabetContain(lab : SymbolicHeap) : Boolean

  /**
    * Evaluates the transition function on the given src, trg, and SH; only meant for evaluating single transitions;
    * to iterate over defined transitions for a fixed SH, use getDefinedTransitions
    */
  def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
    if (implementsTargetComputation) {
      val targets = getTargetsFor(src, lab)
      val res = targets.contains(trg)
      logger.debug("Transition " + src.mkString(", ") + " --[" + lab + "]--> " + trg + " : " + res)
      res
    } else throw new NotImplementedException
  }

  /**
    * Returns all the state sequences on which transitions are defined for the given SH.
    * By default, this is implemented naively in terms of isTransitionDefined. Override if more efficient implementation exists for your SH
    */
  def getDefinedTransitions(lab : SymbolicHeap) : Set[(Seq[State], State)] = {
    val sizeOfSig = lab.pointers.length
    val srcSeqs : Set[Seq[State]] = Combinators.allSeqsOfLength(sizeOfSig, states)

    for {
      src <- srcSeqs
      trg <- states
      if isTransitionDefined(src, trg, lab)
    } yield (src, trg)
  }

  /**
    * Does this automaton implement direct target computation, i.e., can it compute target states from a given sequence of source states and a given symbolic heap?
    */
  def implementsTargetComputation: Boolean = false

  def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = throw new NotImplementedError("No smart target computation for this heap automaton")

  override def toString = description

}
