package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.refinement.AutomatonTask
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 10/15/16.
  */
object ToyExampleAutomata {

  /**
    * An automaton that reaches a final state iff there is at least one points-to assertion
    */
  lazy val HasPointerAutomaton = new HeapAutomaton with HarrshLogging {

    override val description: String = AutomatonTask.keywords.hasptr

    override type State = Boolean

    override val states: Set[State] = Set(true, false)

    override def isFinal(s: State): Boolean = s

    // No restrictions regarding the SH
    override def doesAlphabetContain(lab: SymbolicHeap): Boolean = true

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      val res = trg match {
        case false =>
          // Trg false only if all sources are false and the given SH does not contain a pointer
          !src.exists(b => b) && !lab.hasPointer
        case true =>
          // The converse
          src.exists(b => b) || lab.hasPointer
      }
      logger.debug("Transition " + src.mkString(", ") + "--[" + lab + "]-->" + trg + " : " + res)
      res
    }

  }

  lazy val EvenAutomaton = moduloAutomaton(0,2)

  def moduloAutomaton(remainder : Int, divisor : Int) = new HeapAutomaton with HarrshLogging {

    override val description: String = AutomatonTask.keywords.mod + "(" + remainder + ", " + divisor + ")"

    override type State = Int

    override val states: Set[State] = Set() ++ (0 until divisor)

    override def isFinal(s: State): Boolean = s == remainder

    // No restrictions regarding the SH
    override def doesAlphabetContain(lab: SymbolicHeap): Boolean = true

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      val sum = (src.sum + lab.pointers.size) % divisor
      val res = sum == trg
      logger.debug("Transition " + src.mkString(", ") + "--[" + lab + "]-->" + trg + " : " + res)
      res
    }

    override def implementsTargetComputation: Boolean = true

    override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
      Set((src.sum + lab.pointers.size) % divisor)
    }

  }

}
