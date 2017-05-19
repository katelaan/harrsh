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
    case RunHasPointer(negate : Boolean) => ToyExampleAutomata.hasPointerAutomaton(negate)
    case RunModulo(remainder : Int, divisor : Int, negate : Boolean) => ToyExampleAutomata.moduloAutomaton(remainder, divisor, negate)
    case RunExactTracking(alloc, pure, negate) => TrackingAutomata.singleTargetStateTracking(numFV, alloc, pure, negate)
    case RunRelaxedTracking(alloc, pure, negate) => TrackingAutomata.subsetTracking(numFV, alloc, pure, negate)
    case RunAllocationTracking(alloc, negate) => TrackingAutomata.allocTracking(numFV, alloc, negate)
    case RunPureTracking(pure, negate) => TrackingAutomata.pureTracking(numFV, pure, negate)
    case RunSat() => TrackingAutomata.satAutomaton(numFV)
    case RunUnsat() => TrackingAutomata.unsatAutomaton(numFV)
    case RunEstablishment() => TrackingAutomata.establishmentAutomaton(numFV)
    case RunNonEstablishment() => TrackingAutomata.nonEstablishmentAutomaton(numFV)
    case RunReachability(from, to, negate) => TrackingAutomata.reachabilityAutomaton(numFV, from, to, negate)
    case RunGarbageFreedom() => TrackingAutomata.garbageFreedomAutomaton(numFV)
    case RunWeakAcyclicity() => TrackingAutomata.weakAcyclicityAutomaton(numFV)
    case RunMayHaveGarbage() => TrackingAutomata.mayHaveGarbageAutomaton(numFV)
    case RunStrongCyclicity() => TrackingAutomata.strongCyclicityAutomaton(numFV)
  }

  // TODO Finish complementation
  def complement : AutomatonTask = this match {
    case RunHasPointer(negate : Boolean) => RunHasPointer(!negate)
    case RunModulo(remainder, divisor, negate : Boolean) =>
      if (divisor == 2) RunModulo(1-remainder, divisor, negate) else RunModulo(remainder, divisor, !negate)
    case RunAllocationTracking(alloc, negate) => RunAllocationTracking(alloc, !negate)
    case RunPureTracking(pure, negate) => RunPureTracking(pure, !negate)
    case RunExactTracking(alloc, pure, negate) => RunExactTracking(alloc, pure, !negate)
    case RunRelaxedTracking(alloc, pure, negate) => RunRelaxedTracking(alloc, pure, !negate)
    case RunSat() => RunUnsat()
    case RunUnsat() => RunSat()
    case RunEstablishment() => RunNonEstablishment()
    case RunNonEstablishment() => RunEstablishment()
    case RunReachability(from, to, negate) => RunReachability(from, to, !negate)
    case RunGarbageFreedom() => RunMayHaveGarbage()
    case RunWeakAcyclicity() => RunStrongCyclicity()
    case RunMayHaveGarbage() => RunGarbageFreedom()
    case RunStrongCyclicity() => RunWeakAcyclicity()
  }

  private def negationPrefix(negate : Boolean) = if (negate) "~" else ""

  override def toString: String = this match {
    case RunHasPointer(negate : Boolean) => negationPrefix(negate) + AutomatonTask.keywords.hasptr
    case RunModulo(remainder : Int, divisor : Int, negate : Boolean) => (remainder, divisor, negate) match {
      case (1, 2, false) => AutomatonTask.keywords.odd
      case (0, 2, false) => AutomatonTask.keywords.even
      case _ => negationPrefix(negate) + AutomatonTask.keywords.mod + "[" + remainder + "," + divisor + "]"
    }
    case RunSat() => AutomatonTask.keywords.sat
    case RunUnsat() => AutomatonTask.keywords.unsat
    case RunEstablishment() => AutomatonTask.keywords.est
    case RunNonEstablishment() => AutomatonTask.keywords.nonest
    case RunGarbageFreedom() => AutomatonTask.keywords.gf
    case RunWeakAcyclicity() => AutomatonTask.keywords.acyc
    case RunMayHaveGarbage() => AutomatonTask.keywords.garb
    case RunStrongCyclicity() => AutomatonTask.keywords.cyc
    case RunReachability(from, to, negate) => negationPrefix(negate) + AutomatonTask.keywords.reach + "[" + Var.toDefaultString(from) + "," + Var.toDefaultString(to) + "]"
    case RunExactTracking(alloc, pure, negate) => negationPrefix(negate) + AutomatonTask.keywords.track + "[" + alloc.map(Var.toDefaultString).mkString(",") + (if (pure.nonEmpty) " : " + pure.mkString(",") else "") + "]"
    case RunRelaxedTracking(alloc, pure, negate) => negationPrefix(negate) + AutomatonTask.keywords.reltrack + "[" + alloc.map(Var.toDefaultString).mkString(",") + (if (pure.nonEmpty) " : " + pure.mkString(",") else "") + "]"
    case RunAllocationTracking(alloc, negate) => negationPrefix(negate) + AutomatonTask.keywords.alloc + "[" + alloc.map(Var.toDefaultString).mkString(",") + "]"
    case RunPureTracking(pure, negate) => negationPrefix(negate) + AutomatonTask.keywords.pure + "[" + pure.mkString(",") + "]"
  }

  def resultToString(isEmpty : Boolean) : String = this match {
    case RunHasPointer(negate : Boolean) => if (isEmpty != negate) "no alloc" else "alloc"
    case RunModulo(remainder : Int, divisor : Int, negate : Boolean) => (if (isEmpty) "all" else "ex.") + " #ptr " + (if (isEmpty != negate) "!= " else "== ") + remainder + "%" + divisor
    // TODO Other strings for negated automata?
    case RunExactTracking(alloc, pure, negate) => if (isEmpty) "no matching unf." else "ex. matching unf."
    case RunRelaxedTracking(alloc, pure, negate) => if (isEmpty) "no superset unf." else "ex. superset unf."
    case RunAllocationTracking(alloc, negate) => if (isEmpty) "no superset unf." else "ex. superset unf."
    case RunPureTracking(pure, negate) => if (isEmpty) "no superset unf." else "ex. superset unf."
    case RunSat() => if (isEmpty) "all unsat" else "ex. sat"
    case RunUnsat() => if (isEmpty) "all sat" else "ex. unsat"
    case RunEstablishment() => if (isEmpty) "all non-est." else "ex. est."
    case RunNonEstablishment() => if (isEmpty) "all est." else "ex. non-est"
    case RunReachability(from, to, negate) => if (isEmpty != negate) "all unreach" else "ex. reach"
    case RunGarbageFreedom() => if (isEmpty) "all garbage" else "ex. garbage free"
    case RunWeakAcyclicity() => if (isEmpty) "all cyclic" else "ex. weak. acyc."
    case RunMayHaveGarbage() => if (isEmpty) "all garb. free" else "may ex. garb."
    case RunStrongCyclicity() => if (isEmpty) "all weak. acyc" else "ex. strong. cyc."
  }

}

case class RunHasPointer(negate : Boolean = false) extends AutomatonTask

case class RunModulo(remainder : Int, divisor : Int, negate : Boolean = false) extends AutomatonTask {
  assert(remainder < divisor)
}

case class RunAllocationTracking(alloc : Set[Var], negate : Boolean = false) extends AutomatonTask

case class RunPureTracking(pure : Set[PureAtom], negate : Boolean = false) extends AutomatonTask

case class RunExactTracking(alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) extends AutomatonTask

case class RunRelaxedTracking(alloc : Set[Var], pure : Set[PureAtom], negate : Boolean = false) extends AutomatonTask

case class RunSat() extends AutomatonTask

case class RunUnsat() extends AutomatonTask

case class RunEstablishment() extends AutomatonTask

case class RunNonEstablishment() extends AutomatonTask

case class RunReachability(from : Var, to : Var, negate : Boolean = false) extends AutomatonTask

case class RunGarbageFreedom() extends AutomatonTask

case class RunWeakAcyclicity() extends AutomatonTask

case class RunMayHaveGarbage() extends AutomatonTask

case class RunStrongCyclicity() extends AutomatonTask

object AutomatonTask {

  // TODO Clean this up a little bit. Is getting very long + somewhat repetitive
  def fromString(s : String) : Option[AutomatonTask] = s match {
    case "" => None
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

        def trackParsing(constructor : (Set[Var], Set[PureAtom], Boolean) => AutomatonTask, kw : String)(input : String) : Option[AutomatonTask] =
          for {
            input <- removeSurroundingKeyword(other, kw)
            params : Array[String] = input.split(":")
            fvparams : Array[String] = params(0).trim.split(",")
            pureparams : Array[String] = if (params.size > 1) params(1).trim.split(",") else Array.empty
            if fvparams.forall(isFV)
            fvs = (fvparams map stringToFV).toSet
            eqs = (pureparams map parsePure).toSet
          } yield constructor(fvs, eqs, false)

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

    val (l,r)= (Var.stringToFV(params(0).trim), Var.stringToFV(params(1).trim))
    if (isEq) PtrEq(l,r) else PtrNEq(l,r)
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