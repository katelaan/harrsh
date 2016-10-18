package slex.heapautomata

import slex.Combinators
import slex.heapautomata.utils.{EqualityUtils, UnsafeAtomsAsClosure}
import slex.main._
import slex.seplog._

/**
  * Created by jens on 10/16/16.
  */
object TrackingAutomata extends SlexLogging {

  /**
    * Get tracking automaton for the given number of free variables, whose target states are defined by alloc and pure.
    * @param numFV
    * @param alloc
    * @param pure
    * @return
    */
  def apply(numFV : Int, alloc : Set[FV], pure : Set[PureAtom]) = new HeapAutomaton with SlexLogging {

    override val description: String = "TRACK_" + numFV + "(" +alloc + ", " + pure + ")"

    override type State = (Set[FV], Set[PureAtom])

    private lazy val allFVs = (0 to numFV) map fv
    private lazy val allEQs = allEqualitiesOverFVs(numFV)

    lazy val InconsistentState : State = (Set(), Set() ++ allFVs map (fv => PtrNEq(fv,fv)))

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

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      val targets = getTargetsFor(src, lab)
      val res = targets.contains(trg)
      logger.debug("Transition " + src.mkString(", ") + " --[" + lab + "]--> " + trg + " : " + res)
      res
    }

    override def implementsTargetComputation: Boolean = true

    override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
      logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
      if (src.length != lab.calledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.calledPreds.length + " does not match arity of source state sequence " + src.length)

      val compressed = compress(lab, src)
      logger.debug("Compressed " + lab + " into " + compressed)

      // Compute allocation set and equalities for compressed SH and compare to target
      val allocExplicit: Seq[FV] = compressed.pointers map (_.from)
//      if (HeapAutomataSafeModeEnabled) {
//        if (allocExplicit.distinct != allocExplicit) throw new IllegalStateException(allocExplicit + " contains duplicates")
//      }

      // FIXME: Can we already assume that constraints returned by compression are ordered and thus drop this step?
      val pureExplicit : Set[PureAtom] =  Set() ++ compressed.ptrEqs map orderedAtom

      // Add inequalities for allocated variables
      val inequalitiesFromAlloc : Seq[PureAtom] = Combinators.square(allocExplicit) map {
        case (l,r) => orderedAtom(l, r, false)
      }
      val pureWithAlloc : Set[PureAtom] = pureExplicit ++ inequalitiesFromAlloc

      // Compute fixed point of inequalities and fill up alloc info accordingly
      val stateWithClosure : State = EqualityUtils.propagateConstraints(allocExplicit.toSet, pureWithAlloc)
      logger.debug("State for compressed SH: " + stateWithClosure)
      // If the state is inconsistent, return the unique inconsistent state
      val consistencyCheckedState = checkConsistency(stateWithClosure, InconsistentState)

      // Break state down to only the free variables; the other information is not kept in the state space
      val computedTrg : State = EqualityUtils.dropNonFreeVariables(consistencyCheckedState._1, consistencyCheckedState._2)
      if (stateWithClosure != computedTrg) // TODO: Note that this is quite an expensive comparison that should be removed for evaluation
        logger.debug("After consistency check + dropping bound variables: " + computedTrg)

      // There is a unique target state because we always compute the congruence closure
      Set(computedTrg)
    }

  }

  def compress(sh : SymbolicHeap, qs : Seq[(Set[FV], Set[PureAtom])]) : SymbolicHeap = {
    val shFiltered = sh.removeCalls
    val newHeaps = qs map kernel
    val stateHeapPairs = sh.getCalls zip newHeaps
    val renamedHeaps : Seq[SymbolicHeap] = stateHeapPairs map {
      case (call, sh) =>
        // Rename the free variables of SH to the actual arguments of the predicate calls,
        // i.e. replace the i-th FV with the call argument at index i-1
        val pairs : Seq[(String,String)] = ((1 to call.args.length) map (x => fv(x).toString)) zip (call.args map (_.toString))
        val map : Map[String,String] = Map() ++ pairs
        sh.renameVars(MapBasedRenaming(map))
    }
    val combined = SymbolicHeap.combineAllHeaps(shFiltered +: renamedHeaps)
    combined
  }

  def kernel(s : (Set[FV], Set[PureAtom])) : SymbolicHeap = {
    // FIXME: Here we now assume that the state already contains a closure. If this is not the case, the following does not work.
    //val closure = new ClosureOfAtomSet(pure)
    val closure = UnsafeAtomsAsClosure(s._2)

    val nonredundantAlloc = s._1 filter (closure.isMinimumInItsClass(_))

    val alloc : Set[SpatialAtom] = nonredundantAlloc map (p => ptr(p, nil))

    val res = SymbolicHeap(s._2.toSeq, alloc.toSeq)
    logger.debug("Converting " + s + " to " + res)
    res
  }

  def checkConsistency(s : (Set[FV], Set[PureAtom]), inconsistentState : (Set[FV], Set[PureAtom])) : (Set[FV], Set[PureAtom]) = {
    if (s._2.find({
      // Find inequality with two identical arguments
      case PtrNEq(l, r) if l == r => true
      case _ => false
    }).isDefined) {
      // Inconsistent, return unique inconsistent state
      inconsistentState
    } else {
      // Consistent, return as is
      s
    }
  }

}
