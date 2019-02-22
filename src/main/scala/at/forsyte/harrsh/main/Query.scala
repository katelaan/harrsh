package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.EntailmentInstance
import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.parsers.QueryParser
import at.forsyte.harrsh.refinement.AutomatonTask
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.seplog.sidtransformers.QueryToEntailmentInstance
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

sealed trait Query {
  val status: ProblemStatus
  val fileName: Option[String]

  def asSatQuery: Option[SatQuery] = this match {
    case q: SatQuery => Some(q)
    case _ => None
  }

  def asRefinemetQuery: Option[RefinementQuery] = this match {
    case q: RefinementQuery => Some(q)
    case _ => None
  }

  def toEntailmentInstance(computeSeparateSidsForEachSide: Boolean): Option[EntailmentInstance] = this match {
    case q: EntailmentQuery =>
      QueryToEntailmentInstance(q, computeSeparateSidsForEachSide)
    case _ => None
  }
}

case class RefinementQuery(sid: Sid, task: Option[AutomatonTask], override val status: ProblemStatus, override val fileName: Option[String]) extends Query {

  def setTask(task: AutomatonTask): RefinementQuery = copy(task = Some(task))

  def automaton: HeapAutomaton = task.map(_.getAutomaton).getOrElse{
    throw new Exception("No task specified for refinement => Can't select automaton")
  }

  lazy val taskString: String = task.map(_.toString).getOrElse("(unspecified)")
  val fileNameString: String = fileName.getOrElse("(no file)")

}

object RefinementQuery {

  def fromTaskSpecString(s : String) : Option[RefinementQuery] = {
    // TODO Use Validation instead
    val parts = s.split(";").map(_.trim)

    val res = if (parts.length == 3) {
      val fileName = parts(0)
      val optDecProb = AutomatonTask.fromString(parts(1))
      val expRes = parts(2) match {
        case "false" => ProblemStatus.Incorrect
        case "true" => ProblemStatus.Correct
        case _ => ProblemStatus.Unknown
      }

      val sid = QueryParser.getSidFromFile(fileName)

      optDecProb map (task => RefinementQuery(sid, Some(task), expRes, Some(fileName)))
    } else {
      None
    }

    if (res.isEmpty) {
      println("Error: Failed parsing '" + s + "'")
    }

    res
  }

  def apply(fileName: String, prop: AutomatonTask): RefinementQuery = {
    RefinementQuery(QueryParser.getSidFromFile(fileName), Some(prop), ProblemStatus.Unknown, Some(fileName))
  }
}

case class SatQuery(sid: Sid, query: SymbolicHeap, override val status: ProblemStatus, override val fileName: Option[String]) extends Query {

  val StartPred = "ASSERT"

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("SatBenchmark {\n")
    sb.append("  SID = {\n")
    for (line <- sid.toString.lines) sb.append(s"    $line\n")
    sb.append("  }\n  Query = {\n")
    for (line <- query.toString.lines) sb.append(s"    $line\n")
    sb.append(s"  }\n  Status = $status\n")
    sb.append("}")
    sb.mkString
  }

  /**
    * Integrate the top-level query with the predicates by making the query the start predicate.
    * @return Combined SID
    */
  def toIntegratedSid: Sid = {
    // TODO: Make sure that all automata deal correctly with top-level formulas
    startRule match {
      case None =>
        // The query is a single predicate call => Extract start predicate from that
        val startPred = query.predCalls.head.name
        Sid(startPred, sid.preds, sid.description)
      case Some(rule) =>
        // Need an additional rule to represent query => Derive integrated SID from that
        val allPreds = Predicate(StartPred, Seq(rule)) +: sid.preds
        Sid(StartPred, allPreds, sid.description)
    }

  }

  private def startRule: Option[RuleBody] = {
    if (isRedundantSingleCall(query))
      None
    else
      Some(RuleBody(Nil, query))
  }

  private def isRedundantSingleCall(heap: SymbolicHeap) = {
    if (heap.hasPointer || heap.pure.nonEmpty || heap.predCalls.size != 1) {
      // Query isn't even a single call
      false
    } else {
      // It's a single call => Check if it's redundant...
      val call = heap.predCalls.head
      // ...i.e. if the call does *not* contain null + its args are pairwise different
      !call.args.exists(_.isNull) && call.args.toSet.size == call.args.size
    }
  }

}

case class EntailmentQuery(lhs: SymbolicHeap, rhs: SymbolicHeap, sid: Sid, override val status: ProblemStatus, override val fileName: Option[String]) extends Query {
  def setFileName(fileName: String): EntailmentQuery = copy(fileName = Some(fileName))

}

object EntailmentQuery {

  implicit val entailmentBenchmarkToLatex: ToLatex[EntailmentQuery] = (epr: EntailmentQuery, naming: Naming) => {
    val query = "Check entailment $" + epr.lhs.toLatex(naming) + " \\models " + epr.rhs.toLatex(naming) + "$"
    val sid = epr.sid.toLatex(naming)
    query + "\n%\n" + "w.r.t.\n%\n" + sid + "\n"
  }

}