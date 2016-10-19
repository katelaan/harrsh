package slex.heapautomata

import slex.Combinators
import slex.heapautomata.BaseReachabilityAutomaton.ReachabilityInfo
import slex.heapautomata.BaseTrackingAutomaton.TrackingInfo
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

  def establishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, true)

  def nonEstablishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, false)

  // TODO The reachability automaton would be nicer if the Unit didn't show up all over the place...
  // TODO In general, the paramaterization of the reachability automata is very ugly / spaghetti code
  def reachabilityAutomaton(numFV : Int, from : FV, to : FV) = new BaseReachabilityAutomaton[Unit](
    numFV,
    isFinalPredicate = (_ : BaseReachabilityAutomaton[Unit], ri : ReachabilityInfo, _ : Unit) => ri._2.isReachable(from, to),
    tagComputation = (_, _, _, _) => (),
    inconsistentTag = (),
    valsOfTag = Set(()),
    description = "REACH_" + numFV)

  def garbageFreedomAutomaton(numFV : Int) = new BaseReachabilityAutomaton[Boolean](
    numFV,
    isFinalPredicate = (_, _, tag : Boolean) => tag,
    tagComputation = (tags : Seq[Boolean], ti : TrackingInfo, pairs : Set[(FV,FV)], vars : Set[FV]) => !tags.exists(!_) && BaseReachabilityAutomaton.isGarbageFree(ti, pairs, vars + NullPtr(), numFV),
    inconsistentTag = true, // An inconsistent heap is garbage free
    valsOfTag = Set(true, false),
    description = "GF_" + numFV)

  def acyclicityAutomaton(numFV : Int) = new BaseReachabilityAutomaton[Boolean](
    numFV,
    isFinalPredicate = (_, _, tag : Boolean) => tag,
    tagComputation = (tags : Seq[Boolean], ti : TrackingInfo, pairs : Set[(FV,FV)], vars : Set[FV]) => !tags.exists(!_) && BaseReachabilityAutomaton.isAcyclic(ti, pairs, vars + NullPtr(), numFV),
    inconsistentTag = true, // An inconsistent heap is acyclic
    valsOfTag = Set(true, false),
    description = "ACYC_" + numFV)

}
