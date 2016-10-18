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
    * Get tracking automaton for the given number of free variables, whose target state is specified by alloc and pure.
    */
  def singleTargetStateTracking(numFV : Int, alloc : Set[FV], pure : Set[PureAtom]) = new BaseTrackingAutomaton(
    numFV,
    (_ : BaseTrackingAutomaton, sAlloc : Set[FV], sPure : Set[PureAtom]) => sAlloc == alloc && sPure == pure,
    "TRACK_" + numFV + "(" + alloc + ", " + pure + ")"
  )

  def satAutomaton(numFV : Int) = new BaseTrackingAutomaton(
    numFV,
    (self : BaseTrackingAutomaton, sAlloc : Set[FV], sPure : Set[PureAtom]) => !(sAlloc == self.InconsistentState._1 && sPure == self.InconsistentState._2),
    "SAT_" + numFV
  )

  def unsatAutomaton(numFV : Int) = new BaseTrackingAutomaton(
    numFV,
    (self : BaseTrackingAutomaton, sAlloc : Set[FV], sPure : Set[PureAtom]) => sAlloc == self.InconsistentState._1 && sPure == self.InconsistentState._2,
    "UNSAT_" + numFV
  )

  def establishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV)


}
