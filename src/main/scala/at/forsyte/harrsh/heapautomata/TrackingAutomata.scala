package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.heapautomata.BaseReachabilityAutomaton.ReachabilityInfo
import at.forsyte.harrsh.heapautomata.BaseTrackingAutomaton.TrackingInfo
import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jens on 10/16/16.
  */
object TrackingAutomata extends SlexLogging {

  /**
    * Get tracking automaton for the given number of free variables, whose target state is specified by alloc and pure.
    */
  def singleTargetStateTracking(numFV : Int, alloc : Set[Var], pure : Set[PureAtom]) = new BaseTrackingAutomaton(
    numFV,
    (_ : BaseTrackingAutomaton, sAlloc : Set[Var], sPure : Set[PureAtom]) => sAlloc == alloc && sPure == pure,
    "TRACK_" + numFV + "(" + alloc + ", " + pure + ")"
  )

  def satAutomaton(numFV : Int) = new BaseTrackingAutomaton(
    numFV,
    (self : BaseTrackingAutomaton, sAlloc : Set[Var], sPure : Set[PureAtom]) => !(sAlloc == self.InconsistentState._1 && sPure == self.InconsistentState._2),
    "SAT_" + numFV
  )

  def unsatAutomaton(numFV : Int) = new BaseTrackingAutomaton(
    numFV,
    (self : BaseTrackingAutomaton, sAlloc : Set[Var], sPure : Set[PureAtom]) => sAlloc == self.InconsistentState._1 && sPure == self.InconsistentState._2,
    "UNSAT_" + numFV
  )

  def establishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, true)

  def nonEstablishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, false)

  // TODO The reachability automaton would be nicer if Unit didn't show up all over the place...
  // TODO In general, the paramaterization of the reachability automata is very ugly / spaghetti code
  // TODO Make sure to <= numFV...
  def reachabilityAutomaton(numFV : Int, from : Var, to : Var) = new BaseReachabilityAutomaton[Unit](
    numFV,
    isFinalPredicate = (_ : BaseReachabilityAutomaton[Unit], ri : ReachabilityInfo, _ : Unit) => ri._2.isReachable(from, to),
    tagComputation = (_, _, _, _) => (),
    inconsistentTag = (),
    valsOfTag = Set(()),
    description = "REACH_" + numFV)

  def garbageFreedomAutomaton(numFV : Int) = new BaseReachabilityAutomaton[Boolean](
    numFV,
    isFinalPredicate = (_, _, tag : Boolean) => tag,
    tagComputation = (tags : Seq[Boolean], ti : TrackingInfo, pairs : Set[(Var,Var)], vars : Set[Var]) => !tags.exists(!_) && BaseReachabilityAutomaton.isGarbageFree(ti, pairs, vars + mkVar(0), numFV),
    inconsistentTag = true, // An inconsistent heap is garbage free
    valsOfTag = Set(true, false),
    description = "GF_" + numFV)

  def weakAcyclicityAutomaton(numFV : Int) = new BaseReachabilityAutomaton[Boolean](
    numFV,
    isFinalPredicate = (_, _, tag : Boolean) => tag,
    tagComputation = (tags : Seq[Boolean], ti : TrackingInfo, pairs : Set[(Var,Var)], vars : Set[Var]) => !tags.exists(!_) && BaseReachabilityAutomaton.isAcyclic(ti, pairs, vars + mkVar(0), numFV),
    inconsistentTag = true, // An inconsistent heap is acyclic
    valsOfTag = Set(true, false),
    description = "ACYC_" + numFV)

  // TODO Are the following negations of the reachability-based automata correct?
  // TODO If so, we can reduce code duplication (only differences are in tagComputation and description)

  def mayHaveGarbageAutomaton(numFV : Int) = new BaseReachabilityAutomaton[Boolean](
    numFV,
    isFinalPredicate = (_, _, tag : Boolean) => tag,
    tagComputation = (tags : Seq[Boolean], ti : TrackingInfo, pairs : Set[(Var,Var)], vars : Set[Var]) => tags.exists(b => b) || !BaseReachabilityAutomaton.isGarbageFree(ti, pairs, vars + mkVar(0), numFV),
    inconsistentTag = true, // An inconsistent heap has garbage
    valsOfTag = Set(true, false),
    description = "GARB_" + numFV)

  def strongCyclicityAutomaton(numFV : Int) = new BaseReachabilityAutomaton[Boolean](
    numFV,
    isFinalPredicate = (_, _, tag : Boolean) => tag,
    tagComputation = (tags : Seq[Boolean], ti : TrackingInfo, pairs : Set[(Var,Var)], vars : Set[Var]) => tags.exists(b => b) || !BaseReachabilityAutomaton.isAcyclic(ti, pairs, vars + mkVar(0), numFV),
    inconsistentTag = true, // An inconsistent heap is cyclic
    valsOfTag = Set(true, false),
    description = "CYC_" + numFV)

}
