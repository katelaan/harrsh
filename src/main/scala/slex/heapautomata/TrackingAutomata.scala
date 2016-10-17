package slex.heapautomata

import slex.Combinators
import slex.main._
import slex.seplog.{NullPtr, PointsTo, PureAtom, SpatialAtom, SymbolicHeap}

import scala.annotation.tailrec

/**
  * Created by jens on 10/16/16.
  */
object TrackingAutomata {

  /**
    * Get tracking automaton for the given number of free variables, whose target states are defined by alloc and pure.
    * @param numFV
    * @param alloc
    * @param pure
    * @return
    */
  def apply(numFV : Int, alloc : Set[FV], pure : Set[PureAtom]) = new HeapAutomaton with SlexLogging {

    override val description: String = "TRACK(" + numFV + ")"

    private lazy val allFVs = 0 to numFV
    private lazy val allEQs = allFVEqualities(numFV)

    override type State = (Set[FV], Set[PureAtom])

    override lazy val states: Set[State] = {
      for {
        // TODO: This also computes plenty (but not all) inconsistent states
        alloc <- Combinators.powerSet(Set() ++ ((1 to numFV) map fv))
        pure <- Combinators.powerSet(allEQs)
      } yield (alloc, pure)
    }

    override def isFinal(s: State): Boolean = s._1 == alloc && s._2 == pure

    // TODO Restriction regarding number of FVs
    override def isDefinedOn(lab: SymbolicHeap): Boolean = true

    private def kernel(s : State) : SymbolicHeap = {

      val pure = s._2

      val closure = new Closure(s._2)

      val nonredundantAlloc = s._1 filter (closure.isMinimumInItsClass(_))

      val alloc : Set[SpatialAtom] = nonredundantAlloc map (p => PointsTo(p, NullPtr()))

      val res = SymbolicHeap(pure.toSeq, alloc.toSeq)

      logger.debug("Converting " + s + " to " + res)

      res
    }

    private def compress(sh : SymbolicHeap, qs : Seq[State]) : SymbolicHeap = {
      val shFiltered = sh.removeCalls
      val newHeaps = qs map kernel
      val combined = SymbolicHeap.combineAllHeaps(shFiltered +: newHeaps)
      combined
    }

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      if (src.length != lab.calledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.calledPreds.length + " does not match arity of source state sequence " + src.length)

      // FIXME: Renaming of parameters into args necessary!
      val compressed = compress(lab, src)
      logger.debug("Compressed " + lab + " into " + compressed)

      // Compute allocation set and equalities for compressed SH and compare to target
      // FIXME: Should we have sanity checks that they are all distinct?
      val allocExplicit: Seq[FV] = compressed.pointers map (_.from)
      val pureExplicit : Set[PureAtom] =  Set() ++ compressed.ptrEqs map orderedAtom

      // Add inequalities for allocated variables
      val inequalitiesFromAlloc : Seq[PureAtom] = Combinators.square(allocExplicit) map {
        case (l,r) => orderedAtom(l, r, false)
      }
      val pureWithAlloc : Set[PureAtom] = pureExplicit ++ inequalitiesFromAlloc

      // Compute fixed point of inequalities
      val allPure = propagateConstraints(pureWithAlloc)

      // Propagate equalities to allocation info
      val allocFromEqualities : Set[FV] = propagateEqualitiesToAlloc(allocExplicit.toSet, allPure)

      val shrunkState : State = (allocFromEqualities, allPure)
      logger.debug("State for shrunk SH: " + shrunkState)
      val res = shrunkState == trg

      logger.debug("Transition " + src.mkString(", ") + "--[" + lab + "]-->" + trg + " : " + res)
      res
    }

  }

  private def allFVEqualities(numFV : Int) : Set[PureAtom] = {
    for {
      i <- Set() ++ (0 to numFV-1)
      j <- Set() ++ (i+1 to numFV)
      eq <- Set(true, false)
    } yield orderedAtom(fv(i), fv(j), eq)
  }

  @tailrec
  private def propagateConstraints(from : Set[PureAtom]): Set[PureAtom] = {
    // TODO This function is inefficient

    val newEqs : Seq[PureAtom] = (Combinators.square(from.toIndexedSeq) map {
      case (l,r) => transitiveConstraint(l ,r)
    } filter(_.isDefined) map(_.get))

    val combined = from ++ newEqs
    if (combined == from) from else propagateConstraints(combined)

  }

  private def transitiveConstraint(fvA : PureAtom, fvB : PureAtom) : Option[PureAtom] = {

    val (leftA, rightA, isEqualA) = unwrapAtom(fvA)
    val (leftB, rightB, isEqualB) = unwrapAtom(fvB)

    if (isEqualA || isEqualB) {
      // If at least one is an equality, and one end of the eqs coincides, we can propagate
      val newPair: Option[(FV, FV)] =
        if (leftA == leftB) Some((rightA, rightB))
        else if (leftA == rightB) Some((rightA, leftB))
        else if (rightA == leftB) Some((leftA, rightB))
        else if (rightA == rightB) Some((leftA, leftB))
        else None

      newPair map (p => orderedAtom(p._1, p._2, isEqualA && isEqualB))
    }
    else {
      // Can't infer anything if both are inequalities
      None
    }

  }

  private def propagateEqualitiesToAlloc(explicit: Set[FV], allPure: Set[PureAtom]): Set[FV] = {
    // TODO This is inefficient as well
    val propagated : Set[FV] = (for {
      atom <- allPure
      (l, r, isEq) = unwrapAtom(atom)
      if isEq
      if explicit.contains(l) || explicit.contains(r)
    } yield Set(l,r)).flatten

    explicit union propagated
  }

}
