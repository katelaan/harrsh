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
  def hasPointerAutomaton(negate : Boolean = false) = new HeapAutomaton with HarrshLogging {

    override val description: String = AutomatonTask.keywords.hasptr

    override type State = Boolean

    override def isFinal(s: State): Boolean = s != negate

    /**
      * Direct target computation
      */
    override def getTargetsFor(src: Seq[State], lab: SymbolicHeap): Set[State] = {
      Set(lab.hasPointer || src.exists(b => b))
    }
  }

  lazy val EvenAutomaton = moduloAutomaton(0,2)

  def moduloAutomaton(remainder : Int, divisor : Int, negate : Boolean = false) = new HeapAutomaton with HarrshLogging {

    override val description: String = AutomatonTask.keywords.mod + "(" + remainder + ", " + divisor + ")"

    override type State = Int

    override def isFinal(s: State): Boolean = (s == remainder) != negate

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      val sum = (src.sum + lab.pointers.size) % divisor
      val res = sum == trg
      logger.debug("Transition " + src.mkString(", ") + "--[" + lab + "]-->" + trg + " : " + res)
      res
    }

    override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
      Set((src.sum + lab.pointers.size) % divisor)
    }

  }

}
