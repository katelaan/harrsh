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

  override def toString = this match {
    case RunHasPointer() => "allocates mem"
    case RunTracking(alloc, pure) => "track"
    case RunSat() => "check sat"
    case RunUnsat() => "check unsat"
    case RunEstablishment() => "establishment"
    case RunNonEstablishment() => "non-est."
    case RunReachability(from, to) => "reachability"
    case RunGarbageFreedom() => "garbage-freedom"
    case RunAcyclicity() => "weak acylicity"
  }

  def resultToString(isEmpty : Boolean) : String = this match {
    case RunHasPointer() => if (isEmpty) "no alloc" else "alloc"
    case RunTracking(alloc, pure) => if (isEmpty) "no target" else "target"
    case RunSat() => if (isEmpty) "unsat" else "sat"
    case RunUnsat() => if (isEmpty) "sat" else "unsat"
    case RunEstablishment() => if (isEmpty) "all non-est." else "ex. est."
    case RunNonEstablishment() => if (isEmpty) "all est." else "ex. non-est"
    case RunReachability(from, to) => if (isEmpty) "unreach" else "reach"
    case RunGarbageFreedom() => if (isEmpty) "all garbage" else "ex. garbage-free"
    case RunAcyclicity() => if (isEmpty) "all cyclic" else "ex. weak. acyc."
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