package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.parsers.EntailmentParser.EntailmentParseResult
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

trait EntailmentParser extends JavaTokenParsers with HarrshLogging {

  self: SIDCombinatorParser =>

  final def run(input : String, printFailure : Boolean = true) : Option[EntailmentParseResult] = runParser(parseEntailmentInstance)(input, printFailure)

  def parseEntailmentInstance: Parser[EntailmentParseResult] = parseQuery ~ parseSidGroup ~ opt(parseInfo) ^^ {
    case query ~ sid ~ info =>
      val status = for {
        map <- info
        value <- map.get("status")
        b <- toEntailmentStatus(value)
      } yield b
      EntailmentParseResult(query._1, query._2, sid, status)
  }

  private def toEntailmentStatus(str: String): Option[Boolean] = Try { str.toBoolean }.toOption

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

object EntailmentParser {
  case class EntailmentParseResult(lhs: SymbolicHeap, rhs: SymbolicHeap, sid: SID, entailmentHolds: Option[Boolean])
}