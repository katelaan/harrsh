package slex.heapautomata

import slex.seplog.{PtrExpr, PtrVar}
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

//  def description = this match {
//    case RunHasPointer() => "allocates mem"
//    case RunTracking(alloc, pure) => "track"
//    case RunSat() => "check sat"
//    case RunUnsat() => "check unsat"
//    case RunEstablishment() => "establishment"
//    case RunNonEstablishment() => "non-est."
//    case RunReachability(from, to) => "reachability"
//    case RunGarbageFreedom() => "garbage-freedom"
//    case RunAcyclicity() => "weak acylicity"
//  }

  override def toString = this match {
    case RunHasPointer() => "HASPTR"
    case RunSat() => "SAT"
    case RunUnsat() => "UNSAT"
    case RunEstablishment() => "EST"
    case RunNonEstablishment() => "NON-EST"
    case RunGarbageFreedom() => "GF"
    case RunAcyclicity() => "ACYC"
    case RunReachability(from, to) => "REACH(" + from + "," + to + ")"
    case RunTracking(alloc, pure) => "TRACK(" + alloc.mkString(",") + ")"
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

object AutomatonTask {

  def fromString(s : String) : Option[AutomatonTask] = s match {
    case "SAT" => Some(RunSat())
    case "UNSAT" => Some(RunUnsat())
    case "HASPTR" => Some(RunHasPointer())
    case "EST" => Some(RunEstablishment())
    case "NON-EST" => Some(RunNonEstablishment())
    case "ACYC" => Some(RunAcyclicity())
    case "GF" => Some(RunGarbageFreedom())
    case other =>
      if (other.startsWith("REACH(") && other.endsWith(")")) {
        val params = other.drop(6).init.split(",")
        //println(s + " => " + params.mkString(" : "))

        if (params.size == 2 && isFV(params(0)) && isFV(params(1))) {
          try {
            Some(RunReachability(PtrExpr.fromString(params(0)), PtrExpr.fromString(params(1))))
          } catch {
            case _ : Exception => None
          }
        } else None
      }

      else if (other.startsWith("TRACK(") && other.endsWith(")")) {
        val params = other.drop(6).init.split(",")
        //println(s + " => " + params.mkString(" : "))

        if (!params.exists(!isFV(_))) {
          val fvs : Set[FV] = (params map PtrExpr.fromString).toSet
          Some(RunTracking(fvs, Set()))
        }
        else {
          None
        }
      }

      else None
  }

}