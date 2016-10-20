package slex.heapautomata

import slex.seplog.inductive.PureAtom

/**
  * Created by jkatelaa on 10/20/16.
  */
sealed trait AutomatonTask {

  def getAutomaton(numFV : Int) : HeapAutomaton = this match {
    case RunHasPointer() => ToyExampleAutomata.HasPointerAutomaton
    case RunTracking(alloc, pure) => TrackingAutomata.singleTargetStateTracking(numFV, alloc, pure)
    case RunSat() => TrackingAutomata.satAutomaton(numFV)
    case RunUnsat() => TrackingAutomata.unsatAutomaton(numFV)
    case RunEstablishment() => TrackingAutomata.establishmentAutomaton(numFV)
    case RunNonEstablishment() => TrackingAutomata.nonEstablishmentAutomaton(numFV)
    case RunReachability(from, to) => TrackingAutomata.reachabilityAutomaton(numFV, from, to)
    case RunGarbageFreedom() => TrackingAutomata.garbageFreedomAutomaton(numFV)
    case RunAcyclicity() => TrackingAutomata.acyclicityAutomaton(numFV)
  }

}

case class RunHasPointer() extends AutomatonTask

case class RunTracking(alloc : Set[FV], pure : Set[PureAtom]) extends AutomatonTask

case class RunSat() extends AutomatonTask

case class RunUnsat() extends AutomatonTask

case class RunEstablishment() extends AutomatonTask

case class RunNonEstablishment() extends AutomatonTask

case class RunReachability(from : FV, to : FV) extends AutomatonTask

case class RunGarbageFreedom() extends AutomatonTask

case class RunAcyclicity() extends AutomatonTask