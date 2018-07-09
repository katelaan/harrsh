package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.inductive.{Rule, SID, SymbolicHeap}

case class SatBenchmark(preds: SID, consts: List[String], query: SymbolicHeap, status: SatBenchmark.Status) {

  def StartPred = "ASSERT"

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("SatBenchmark {\n")
    sb.append("  SID = {\n")
    for (line <- preds.toString.lines) sb.append(s"    $line\n")
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
  def toIntegratedSid: SID = {
    val startRule = Rule(StartPred, consts, Nil, query)
    val allRules = startRule +: preds.rules
    // Note: The number of free variables of the top-level query does not matter for the arity of the automaton!
    // TODO: Check that all automata deal correctly with top-level formulas
    SID(StartPred, allRules, preds.description, preds.numFV)
  }

}

object SatBenchmark {

  sealed trait Status {
    override def toString: String = this match {
      case Sat => "sat"
      case Unsat => "unsat"
      case Unknown => "unknown"
    }
  }
  case object Sat extends Status
  case object Unsat extends Status
  case object Unknown extends Status
  object Status {
    def fromString(s : String): Status = s match {
      case "sat" => Sat
      case "unsat" => Unsat
      case "unknown" => Unknown
      case other => throw new Exception(s"Can't convert $other to problem status")
    }
  }

}
