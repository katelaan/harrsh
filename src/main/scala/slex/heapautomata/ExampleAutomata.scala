package slex.heapautomata
import slex.main.SlexLogging
import slex.seplog.SymbolicHeap

/**
  * Created by jens on 10/15/16.
  */
object ExampleAutomata {

  /**
    * An automaton that reaches a final state iff there is at least one points-to assertion
    */
  lazy val HasPointerAutomaton = new HeapAutomaton with SlexLogging {

    override val description: String = "At least one points-to assertion"

    override type State = Boolean

    override val states: Set[State] = Set(true, false)

    override def isFinal(s: State): Boolean = s

    // No restrictions regarding the SH
    override def isDefinedOn(lab: SymbolicHeap): Boolean = true

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      val res = trg match {
        case false =>
          // Trg false only if all sources are false and the given SH does not contain a pointer
          !src.find(b => b).isDefined && !lab.hasPointer
        case true =>
          // The converse
          src.find(b => b).isDefined || lab.hasPointer
      }
      logger.debug("Transition " + src.mkString(", ") + "--[" + lab + "]-->" + trg + " : " + res)
      res
    }

  }

}
