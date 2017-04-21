package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jens on 10/16/16.
  */
object TrackingAutomata extends HarrshLogging {

  def singleTargetStateTracking(numFV : Int, alloc : Set[Var], pure : Set[PureAtom]) = new BaseTrackingAutomaton.TrackingAutomatonWithSingleFinalState(numFV, alloc, pure)
  def subsetTracking(numFV : Int, alloc : Set[Var], pure : Set[PureAtom]) = new BaseTrackingAutomaton.SubsetTrackingAutomaton(numFV, alloc, pure)

  def allocTracking(numFV : Int, alloc : Set[Var]) = new BaseTrackingAutomaton.AllocationTrackingAutomaton(numFV, alloc)
  def pureTracking(numFV : Int, pure : Set[PureAtom]) = new BaseTrackingAutomaton.PureTrackingAutomaton(numFV, pure)

  def satAutomaton(numFV : Int) = new SatAutomaton(numFV, negate = false)
  def unsatAutomaton(numFV : Int) = new SatAutomaton(numFV, negate = true)

  def establishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, true)
  def nonEstablishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, false)

  def reachabilityAutomaton(numFV : Int, from : Var, to : Var) = new ReachabilityAutomaton(numFV, from, to)

  def garbageFreedomAutomaton(numFV : Int) = new GarbageAutomaton(numFV, negate = false)
  def mayHaveGarbageAutomaton(numFV : Int) = new GarbageAutomaton(numFV, negate = true)

  def weakAcyclicityAutomaton(numFV : Int) = new AcyclicityAutomaton(numFV, negate = false)
  def strongCyclicityAutomaton(numFV : Int) = new AcyclicityAutomaton(numFV, negate = true)

}
