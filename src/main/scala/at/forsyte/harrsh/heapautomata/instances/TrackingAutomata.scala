package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jens on 10/16/16.
  */
object TrackingAutomata extends HarrshLogging {

  def singleTargetStateTracking(numFV : Int, alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) = new BaseTrackingAutomaton.TrackingAutomatonWithSingleFinalState(numFV, alloc, pure, negate)
  def subsetTracking(numFV : Int, alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) = new BaseTrackingAutomaton.SubsetTrackingAutomaton(numFV, alloc, pure, negate)

  def allocTracking(numFV : Int, alloc : Set[Var], negate : Boolean = false) = new BaseTrackingAutomaton.AllocationTrackingAutomaton(numFV, alloc, negate)
  def pureTracking(numFV : Int, pure : Set[PureAtom], negate : Boolean = false) = new BaseTrackingAutomaton.PureTrackingAutomaton(numFV, pure, negate)

  def satAutomaton(numFV : Int) = new SatAutomaton(numFV, negate = false)
  def unsatAutomaton(numFV : Int) = new SatAutomaton(numFV, negate = true)

  def establishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, true)
  def nonEstablishmentAutomaton(numFV : Int) = new EstablishmentAutomaton(numFV, false)

  def reachabilityAutomaton(numFV : Int, from : Var, to : Var, negate : Boolean = false) = new ReachabilityAutomaton(numFV, from, to, negate)

  def garbageFreedomAutomaton(numFV : Int) = new GarbageAutomaton(numFV, negate = false)
  def mayHaveGarbageAutomaton(numFV : Int) = new GarbageAutomaton(numFV, negate = true)

  def weakAcyclicityAutomaton(numFV : Int) = new AcyclicityAutomaton(numFV, negate = false)
  def strongCyclicityAutomaton(numFV : Int) = new AcyclicityAutomaton(numFV, negate = true)

}
