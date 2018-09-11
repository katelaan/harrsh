package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.inductive.{Predicate, RuleBody, SID, SymbolicHeap}

case class SatBenchmark(sid: SID, query: SymbolicHeap, status: SatBenchmark.Status) {

  def StartPred = "ASSERT"

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
  def toIntegratedSid: SID = {
    // TODO: Make sure that all automata deal correctly with top-level formulas
    startRule match {
      case None =>
        // The query is a single predicate call => Extract start predicate from that
        val startPred = query.predCalls.head.name
        SID(startPred, sid.preds, sid.description)
      case Some(rule) =>
        // Need an additional rule to represent query => Derive integrated SID from that
        val allPreds = Predicate(StartPred, Seq(rule)) +: sid.preds
        SID(StartPred, allPreds, sid.description)
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
