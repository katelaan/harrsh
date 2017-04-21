package at.forsyte.harrsh.refinement

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.heapautomata.instances.{ToyExampleAutomata, TrackingAutomata}
import at.forsyte.harrsh.seplog.{PtrExpr, Var}
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PtrNEq, PureAtom}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 10/20/16.
  */
sealed trait AutomatonTask {

  // TODO Possibly give Boolean param to SAT, EST etc instead of having two separate case classes?

  def getAutomaton(numFV : Int) : HeapAutomaton = this match {
    case RunHasPointer() => ToyExampleAutomata.HasPointerAutomaton
    case RunModulo(remainder : Int, divisor : Int) => ToyExampleAutomata.moduloAutomaton(remainder, divisor)
    case RunExactTracking(alloc, pure) => TrackingAutomata.singleTargetStateTracking(numFV, alloc, pure)
    case RunRelaxedTracking(alloc, pure) => TrackingAutomata.subsetTracking(numFV, alloc, pure)
    case RunAllocationTracking(alloc) => TrackingAutomata.allocTracking(numFV, alloc)
    case RunPureTracking(pure) => TrackingAutomata.pureTracking(numFV, pure)
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
  override def toString: String = this match {
    case RunHasPointer() => AutomatonTask.keywords.hasptr
    case RunModulo(remainder : Int, divisor : Int) => (remainder, divisor) match {
      case (1, 2) => AutomatonTask.keywords.odd
      case (0, 2) => AutomatonTask.keywords.even
      case _ => AutomatonTask.keywords.mod + "[" + remainder + "," + divisor + "]"
    }
    case RunSat() => AutomatonTask.keywords.sat
    case RunUnsat() => AutomatonTask.keywords.unsat
    case RunEstablishment() => AutomatonTask.keywords.est
    case RunNonEstablishment() => AutomatonTask.keywords.nonest
    case RunGarbageFreedom() => AutomatonTask.keywords.gf
    case RunWeakAcyclicity() => AutomatonTask.keywords.acyc
    case RunMayHaveGarbage() => AutomatonTask.keywords.garb
    case RunStrongCyclicity() => AutomatonTask.keywords.cyc
    case RunReachability(from, to) => AutomatonTask.keywords.reach + "[" + Var.toDefaultString(from) + "," + Var.toDefaultString(to) + "]"
    case RunExactTracking(alloc, pure) => AutomatonTask.keywords.track + "[" + alloc.map(Var.toDefaultString).mkString(",") + (if (pure.nonEmpty) " : " + pure.mkString(",") else "") + "]"
    case RunRelaxedTracking(alloc, pure) => AutomatonTask.keywords.reltrack + "[" + alloc.map(Var.toDefaultString).mkString(",") + (if (pure.nonEmpty) " : " + pure.mkString(",") else "") + "]"
    case RunAllocationTracking(alloc) => AutomatonTask.keywords.alloc + "[" + alloc.map(Var.toDefaultString).mkString(",") + "]"
    case RunPureTracking(pure) => AutomatonTask.keywords.pure + "[" + pure.mkString(",") + "]"
  }

  def resultToString(isEmpty : Boolean) : String = this match {
    case RunHasPointer() => if (isEmpty) "no alloc" else "alloc"
    case RunModulo(remainder : Int, divisor : Int) => (if (isEmpty) "all" else "ex.") + " #ptr " + (if (isEmpty) "!= " else "== ") + remainder + "%" + divisor
    case RunExactTracking(alloc, pure) => if (isEmpty) "no matching unf." else "ex. matching unf."
    case RunRelaxedTracking(alloc, pure) => if (isEmpty) "no superset unf." else "ex. superset unf."
    case RunAllocationTracking(alloc) => if (isEmpty) "no superset unf." else "ex. superset unf."
    case RunPureTracking(pure) => if (isEmpty) "no superset unf." else "ex. superset unf."
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

case class RunAllocationTracking(alloc : Set[Var]) extends AutomatonTask

case class RunPureTracking(pure : Set[PureAtom]) extends AutomatonTask

case class RunExactTracking(alloc : Set[Var], pure : Set[PureAtom]) extends AutomatonTask

case class RunRelaxedTracking(alloc : Set[Var], pure : Set[PureAtom]) extends AutomatonTask

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
    case keywords.sat => Some(RunSat())
    case keywords.unsat => Some(RunUnsat())
    case keywords.hasptr => Some(RunHasPointer())
    case keywords.even => Some(RunModulo(0,2))
    case keywords.odd => Some(RunModulo(1,2))
    case keywords.est => Some(RunEstablishment())
    case keywords.nonest => Some(RunNonEstablishment())
    case keywords.acyc => Some(RunWeakAcyclicity())
    case keywords.cyc => Some(RunStrongCyclicity())
    case keywords.gf => Some(RunGarbageFreedom())
    case keywords.garb => Some(RunMayHaveGarbage())
    case other =>
      val res = Combinators.exceptionToNone("An exception occurred during parsing of " + other) {

        val modResult: Option[AutomatonTask] = for {
          input <- removeSurroundingKeyword(other, keywords.mod)
          params = input.split(",")
          if params.size == 2
          remainder = Integer.parseInt(params(0))
          divisor = Integer.parseInt(params(1))
        } yield RunModulo(remainder, divisor)

        val reachResult = for {
          input <- removeSurroundingKeyword(other, keywords.reach)
          params = input.split(",")
          // TODO Allow variable names as in unparsed SIDs?
          if params.size == 2 && isFV(params(0)) && isFV(params(1))
        } yield RunReachability(stringToFV(params(0)), stringToFV(params(1)))

        val allocResult = for {
          input <- removeSurroundingKeyword(other, keywords.alloc)
          params = input.split(",")
          if params.forall(isFV)
          fvs = (params map stringToFV).toSet
        } yield RunAllocationTracking(fvs)

        val pureResult = for {
          input <- removeSurroundingKeyword(other, keywords.pure)
          params = input.split(",")
          eqs = (params map parsePure).toSet
        } yield RunPureTracking(eqs)

        def trackParsing(constructor : (Set[Var], Set[PureAtom]) => AutomatonTask, kw : String)(input : String) : Option[AutomatonTask] =
          for {
            input <- removeSurroundingKeyword(other, kw)
            params : Array[String] = input.split(":")
            fvparams : Array[String] = params(0).trim.split(",")
            pureparams : Array[String] = if (params.size > 1) params(1).trim.split(",") else Array.empty
            if fvparams.forall(isFV)
            fvs = (fvparams map stringToFV).toSet
            eqs = (pureparams map parsePure).toSet
          } yield constructor(fvs, eqs)

        val trackResult = trackParsing(RunExactTracking, keywords.track)(other)
        val relaxedTrackResult = trackParsing(RunRelaxedTracking, keywords.reltrack)(other)

        val successfulParses = Seq.empty ++ modResult ++ reachResult ++ allocResult ++ pureResult ++ trackResult ++ relaxedTrackResult
        successfulParses.headOption
      }

      if (res.isEmpty) println("Could not parse task " + other)

      res
  }

  private def parsePure(input : String) : PureAtom = {
    val (params, isEq) = if (input.contains("!=")) {
      (input.split("!="), false)
    } else if (input.contains("\u2249")) {
      (input.split("\u2249"), false)
    } else if (input.contains("\u2248")) {
      (input.split("\u2248"), true)
    } else (input.split("="), true)

    (if (isEq) PtrEq else PtrNEq)(PtrExpr.fromFV(Var.stringToFV(params(0).trim)), PtrExpr.fromFV(Var.stringToFV(params(1).trim)))
  }

  private def removeSurroundingKeyword(input : String, kw : String) : Option[String] = {
    if ((input.startsWith(kw + "(") && input.endsWith(")")) || (input.startsWith(kw + "[") && input.endsWith("]"))) {
      Some(input.drop(kw.length + 1).init)
    } else {
      None
    }
  }

  object keywords {
    val hasptr = "HASPTR"
    val mod = "MOD"
    val odd = "ODD"
    val even = "EVEN"
    val track = "TRACK"
    val reltrack = "REL-TR"
    val alloc = "ALLOC"
    val pure = "PURE"
    val sat = "SAT"
    val unsat = "UNSAT"
    val est = "EST"
    val nonest = "NON-EST"
    val gf = "GF"
    val garb = "GARB"
    val acyc = "ACYC"
    val cyc = "CYC"
    val reach = "REACH"
  }

}