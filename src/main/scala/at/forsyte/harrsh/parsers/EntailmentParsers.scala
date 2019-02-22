package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.{EntailmentQuery, HarrshLogging}
import at.forsyte.harrsh.parsers.buildingblocks.{AsciiAtoms, EmptyQuantifierPrefix}
import at.forsyte.harrsh.seplog.inductive._

object EntailmentParsers extends HarrshLogging {

  val DefaultEntailmentParser = new EntailmentParser with HarrshSidParser with AsciiAtoms with EmptyQuantifierPrefix

  def parseHarrshEntailmentFormat(input: String): Option[EntailmentQuery] = {
    for {
      pr <- DefaultEntailmentParser.run(input)
      if pr.lhs.predCalls forall (hasCorrectArity(_, pr.sid))
      if pr.rhs.predCalls forall (hasCorrectArity(_, pr.sid))
    } yield pr
  }

  private def hasCorrectArity(call: PredCall, sid: Sid) = {
    val res = call.args.length == sid(call.name).arity
    if (!res) {
      logger.error(s"Invalid input: Query contains call $call, but predicate ${call.name} has arity ${sid(call.name).arity}")
    }
    res
  }

}
