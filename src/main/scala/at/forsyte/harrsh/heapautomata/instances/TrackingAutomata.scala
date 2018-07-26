package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jens on 10/16/16.
  */
object TrackingAutomata extends HarrshLogging {

  def singleTargetStateTracking(alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) = new BaseTrackingAutomaton.TrackingAutomatonWithSingleFinalState(alloc, pure, negate)
  def subsetTracking(alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) = new BaseTrackingAutomaton.SubsetTrackingAutomaton(alloc, pure, negate)

  def allocTracking(alloc : Set[Var], negate : Boolean = false) = new BaseTrackingAutomaton.AllocationTrackingAutomaton(alloc, negate)
  def pureTracking(pure : Set[PureAtom], negate : Boolean = false) = new BaseTrackingAutomaton.PureTrackingAutomaton(pure, negate)

  def satAutomaton = new SatAutomaton(negate = false)
  def unsatAutomaton = new SatAutomaton(negate = true)

  def establishmentAutomaton = new EstablishmentAutomaton(acceptEstablished = true)
  def nonEstablishmentAutomaton = new EstablishmentAutomaton(acceptEstablished = false)

  def reachabilityAutomaton(from : Var, to : Var, negate : Boolean = false) = new ReachabilityAutomaton(from, to, negate)

  def garbageFreedomAutomaton = new GarbageAutomaton(negate = false)
  def mayHaveGarbageAutomaton = new GarbageAutomaton(negate = true)

  def weakAcyclicityAutomaton = new AcyclicityAutomaton(negate = false)
  def strongCyclicityAutomaton = new AcyclicityAutomaton(negate = true)

}
