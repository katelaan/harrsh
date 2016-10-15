package slex.heapautomata
import slex.main.SlexLogging
import slex.seplog.{NullPtr, PointsTo, PtrEq, PtrNEq, SpatialAtom, SymbolicHeap}

import scala.annotation.tailrec

/**
  * Created by jens on 10/15/16.
  */
object ExampleAutomata {

  def fv(ix : Int) = ExampleSIDs.fv(ix)

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

  type FV = Int

  case class FVEquality(left : FV, right : FV, isEqual : Boolean) {

    override def toString = left + (if (isEqual) " \u2248 " else " \u2249 ") + right

    def isEqualityToSmallerVar(v : FV) : Boolean = {
      isEqual && ((left == v && right < left) || (right == v && left < right))
    }

  }

  def allFVEqualities(numFV : Int) : Set[FVEquality] = {
    for {
      i <- Set() ++ (1 to numFV)
      j <- Set() ++ (0 to numFV)
      if i > j
      eq <- Set(true, false)
    } yield FVEquality(i, j, eq)
  }

  def powerSet[A](set : Set[A]) : Set[Set[A]] = {
    val seq = set.toSeq

    // TODO: Rewrite to tailrec
    def powerSet(elems : Seq[A]) : Set[Set[A]] = {
      if (elems.isEmpty)
        Set(Set())
      else {
        val newelem = elems.head
        val smaller = powerSet(elems.tail)
        smaller flatMap (set => Set(set, set + newelem))
      }
    }

    powerSet(seq)
  }

  def trackingAutomaton(numFV : Int, alloc : Set[FV], pure : Set[FVEquality]) = new HeapAutomaton with SlexLogging {

    override val description: String = "TRACK(" + numFV + ")"

    private lazy val allFVs = 0 to numFV
    private lazy val allEQs = allFVEqualities(numFV)

    override type State = (Set[FV], Set[FVEquality])

    override lazy val states: Set[State] = {
      for {
        alloc <- powerSet(Set() ++ (1 to numFV))
        pure <- powerSet(allEQs)
      } yield (alloc, pure)
    }

    override def isFinal(s: State): Boolean = s._1 == alloc && s._2 == pure

    // TODO Restriction regarding number of FVs
    override def isDefinedOn(lab: SymbolicHeap): Boolean = true

    private def representationSH(s : State) : SymbolicHeap = {

      val pure = s._2 map {
        case FVEquality(left, right, isEqual) =>
          if (isEqual) PtrEq(fv(left), fv(right)) else PtrNEq(fv(left), fv(right))
      }
      // TODO: Can improve efficiency here
      val nonredundantAlloc = s._1 filterNot {
        i => s._2.find(_.isEqualityToSmallerVar(i)).isDefined
      }
      val alloc : Set[SpatialAtom] = nonredundantAlloc map (i => PointsTo(fv(i), NullPtr()))

      val res = SymbolicHeap(pure.toSeq, alloc.toSeq)

      logger.debug("Converting " + s + " to " + res)
    }

    private def shrink(sh : SymbolicHeap, qs : Seq[State]) : SymbolicHeap = {
      val shFiltered = sh.removeCalls
      val newHeaps = qs map representationSH
      val combined = SymbolicHeap.combineAllHeaps(shFiltered +: newHeaps)
      logger.debug("Shrinking " + sh + " into " + combined)
      combined
    }

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      val res = ???

      // TODO: Implement the actual transition logic...

      logger.debug("Transition " + src.mkString(", ") + "--[" + lab + "]-->" + trg + " : " + res)
      res
    }

  }
}
