package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.{EntailmentQuery, HarrshLogging, ProblemStatus}
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

trait EntailmentParser extends JavaTokenParsers with HarrshLogging {

  self: SIDCombinatorParser =>

  final def run(input : String, printFailure : Boolean = true) : Option[EntailmentQuery] = runParser(parseEntailmentQuery)(input, printFailure)

  def parseEntailmentQuery: Parser[EntailmentQuery] = parseQuery ~ parseSidGroup ~ opt(parseInfo) ^^ {
    case query ~ sid ~ info =>
      val status = for {
        map <- info
        value <- map.get("status")
      } yield toEntailmentStatus(value)
      EntailmentQuery(query._1, query._2, sid, status.getOrElse(ProblemStatus.Unknown), None)
  }

  private def toEntailmentStatus(str: String): ProblemStatus = Try { str.toBoolean } map {
    if (_) ProblemStatus.Correct else ProblemStatus.Incorrect
  } getOrElse(ProblemStatus.Unknown)

  def parseSidGroup: Parser[SID] = parseGroup("sid"){
    parseSID
  }

  def parseQuery: Parser[(SymbolicHeap, SymbolicHeap)] = parseGroup("query") {
    parseSymbolicHeap ~ ("|=" ~> parseSymbolicHeap)
  } ^^ {
    case lhs ~ rhs => (lhs, rhs)
  }

  def parseInfo: Parser[Map[String, String]] = parseGroup("info") {
    parseKeyValuePair("status") ^^ { Map(_) }
  }

  def parseGroup[A](keyword: String)(parser: Parser[A]): Parser[A] = keyword ~> "{" ~> parser <~ "}"

  def parseKeyValuePair(key: String): Parser[(String, String)] = key ~ ("=" ~> ident) ^^ {
    case k ~ v => (k,v)
  }

}