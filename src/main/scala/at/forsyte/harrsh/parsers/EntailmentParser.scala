package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.{EntailmentQuery, HarrshLogging, InputStatus}
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

trait EntailmentParser extends JavaTokenParsers with HarrshLogging {

  self: SIDCombinatorParser =>

  final def run(input : String, printFailure : Boolean = true) : Option[EntailmentQuery] = runParser(parseEntailmentInstance)(input, printFailure)

  def parseEntailmentInstance: Parser[EntailmentQuery] = parseQuery ~ parseSidGroup ~ opt(parseInfo) ^^ {
    case query ~ sid ~ info =>
      val status = for {
        map <- info
        value <- map.get("status")
      } yield toEntailmentStatus(value)
      EntailmentQuery(query._1, query._2, sid, status.getOrElse(InputStatus.Unknown))
  }

  private def toEntailmentStatus(str: String): InputStatus = Try { str.toBoolean } map {
    if (_) InputStatus.Sat else InputStatus.Unsat
  } getOrElse(InputStatus.Unknown)

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