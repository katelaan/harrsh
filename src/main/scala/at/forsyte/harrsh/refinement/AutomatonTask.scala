package at.forsyte.harrsh.refinement

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.heapautomata.instances.{ToyExampleAutomata, TrackingAutomata}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jkatelaa on 10/20/16.
  */
sealed trait AutomatonTask {

  // TODO Possibly give Boolean param to SAT, EST etc instead of having two separate case classes?

  def getAutomaton(numFV : Int) : HeapAutomaton = this match {
    case RunHasPointer() => ToyExampleAutomata.HasPointerAutomaton
    case RunModulo(remainder : Int, divisor : Int) => ToyExampleAutomata.moduloAutomaton(remainder, divisor)
    case RunTracking(alloc, pure) => TrackingAutomata.singleTargetStateTracking(numFV, alloc, pure)
    case RunSat() => TrackingAutomata.satAutomaton(numFV)
    case RunUnsat() => TrackingAutomata.unsatAutomaton(numFV)
    case RunEstablishment() => TrackingAutomata.establishmentAutomaton(numFV)
    case RunNonEstablishment() => TrackingAutomata.nonEstablishmentAutomaton(numFV)
    case RunReachability(from, to) => TrackingAutomata.reachabilityAutomaton(numFV, from, to)
    case RunGarbageFreedom() => TrackingAutomata.garbageFreedomAutomaton(numFV)
    case RunWeakAcyclicity() => TrackingAutomata.weakAcyclicityAutomaton(numFV)
    case RunMayHaveGarbage() => TrackingAutomata.mayHaveGarbageAutomaton(numFV)
    case RunStrongCyclicity() => TrackingAutomata.strongCyclicityAutomaton(numFV)
  }

  // TODO Code duplication with fromString
  override def toString = this match {
    case RunHasPointer() => "HASPTR"
    case RunModulo(remainder : Int, divisor : Int) => (remainder, divisor) match {
      case (1, 2) => "ODD"
      case (0, 2) => "EVEN"
      case _ => "MOD[" + remainder + "," + divisor + "]"
    }
    case RunSat() => "SAT"
    case RunUnsat() => "UNSAT"
    case RunEstablishment() => "EST"
    case RunNonEstablishment() => "NON-EST"
    case RunGarbageFreedom() => "GF"
    case RunWeakAcyclicity() => "ACYC"
    case RunMayHaveGarbage() => "GARB"
    case RunStrongCyclicity() => "CYC"
    case RunReachability(from, to) => "REACH[" + Var.toDefaultString(from) + "," + Var.toDefaultString(to) + "]"
    case RunTracking(alloc, pure) => "TRACK[" + alloc.map(Var.toDefaultString).mkString(",") + "]"
  }

  def resultToString(isEmpty : Boolean) : String = this match {
    case RunHasPointer() => if (isEmpty) "no alloc" else "alloc"
    case RunModulo(remainder : Int, divisor : Int) => (if (isEmpty) "all" else "ex.") + " #ptr " + (if (isEmpty) "!= " else "== ") + remainder + "%" + divisor
    case RunTracking(alloc, pure) => if (isEmpty) "no target unf." else "ex. target unf."
    case RunSat() => if (isEmpty) "all unsat" else "ex. sat"
    case RunUnsat() => if (isEmpty) "all sat" else "ex. unsat"
    case RunEstablishment() => if (isEmpty) "all non-est." else "ex. est."
    case RunNonEstablishment() => if (isEmpty) "all est." else "ex. non-est"
    case RunReachability(from, to) => if (isEmpty) "all unreach" else "ex. reach"
    case RunGarbageFreedom() => if (isEmpty) "all garbage" else "ex. garbage free"
    case RunWeakAcyclicity() => if (isEmpty) "all cyclic" else "ex. weak. acyc."
    case RunMayHaveGarbage() => if (isEmpty) "all garb. free" else "may ex. garb."
    case RunStrongCyclicity() => if (isEmpty) "all weak. acyc" else "ex. strong. cyc."
  }

}

case class RunHasPointer() extends AutomatonTask

case class RunModulo(remainder : Int, divisor : Int) extends AutomatonTask

case class RunTracking(alloc : Set[Var], pure : Set[PureAtom]) extends AutomatonTask

case class RunSat() extends AutomatonTask

case class RunUnsat() extends AutomatonTask

case class RunEstablishment() extends AutomatonTask

case class RunNonEstablishment() extends AutomatonTask

case class RunReachability(from : Var, to : Var) extends AutomatonTask

case class RunGarbageFreedom() extends AutomatonTask

case class RunWeakAcyclicity() extends AutomatonTask

case class RunMayHaveGarbage() extends AutomatonTask

case class RunStrongCyclicity() extends AutomatonTask

object AutomatonTask {

  // TODO Clean this up a little bit. Is getting very long + somewhat repetitive
  def fromString(s : String) : Option[AutomatonTask] = s match {
    case "SAT" => Some(RunSat())
    case "UNSAT" => Some(RunUnsat())
    case "HASPTR" => Some(RunHasPointer())
    case "EVEN" => Some(RunModulo(0,2))
    case "ODD" => Some(RunModulo(1,2))
    case "EST" => Some(RunEstablishment())
    case "NON-EST" => Some(RunNonEstablishment())
    case "ACYC" => Some(RunWeakAcyclicity())
    case "CYC" => Some(RunStrongCyclicity())
    case "GF" => Some(RunGarbageFreedom())
    case "GARB" => Some(RunMayHaveGarbage())
    case other =>
      if ((other.startsWith("MOD(") && other.endsWith(")")) || (other.startsWith("MOD[") && other.endsWith("]"))) {
        val params = other.drop(4).init.split(",")
        if (params.size == 2) {
          try {
            val remainder = Integer.parseInt(params(0))
            val divisor = Integer.parseInt(params(1))
            Some(RunModulo(remainder,divisor))
          } catch {
            case e : Exception =>
              println("Could not parse modulo task: " + e.getMessage)
              None
          }
        } else None
      }

      else if ((other.startsWith("REACH(") && other.endsWith(")")) || (other.startsWith("REACH[") && other.endsWith("]"))) {
        val params = other.drop(6).init.split(",")
        //println(s + " => " + params.mkString(" : "))

        // TODO Allow variable names as in unparsed source code?
        if (params.size == 2 && isFV(params(0)) && isFV(params(1))) {
          try {
            Some(RunReachability(stringToFV(params(0)), stringToFV(params(1))))
          } catch {
            case e : Exception =>
              println("Could not parse reachability task: " + e.getMessage)
              None
          }
        } else None
      }

      else if ((other.startsWith("TRACK(") && other.endsWith(")")) || (other.startsWith("TRACK[") && other.endsWith("]"))) {
        val params = other.drop(6).init.split(",")
        //println(s + " => " + params.mkString(" : "))

        if (!params.exists(!isFV(_))) {
          val fvs : Set[Var] = (params map stringToFV).toSet
          Some(RunTracking(fvs, Set()))
        }
        else {
          None
        }
      }

      else None
  }

}