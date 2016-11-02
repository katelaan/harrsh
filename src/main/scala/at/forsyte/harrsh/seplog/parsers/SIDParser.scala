package at.forsyte.harrsh.seplog.parsers

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SIDParser extends JavaTokenParsers {

  def parseAtom : Parser[StringSepLogAtom] = parseSpatialAtom | parsePureAtom

  def parseSpatialAtom : Parser[StringSpatialAtom] = parsePointsTo | parseCall | parseEmp

  def parseEmp : Parser[StringEmp] = "emp" ^^ { _ => StringEmp() }

  def parseCall : Parser[StringPredCall] = ident ~ parsePtrSeqInParens ^^ {
    case name ~ args => StringPredCall(name, args)
  }

  def parsePointsSingleTo : Parser[StringPointsTo] = parsePtr ~ ("->" ~> parsePtr) ^^ {
    case l ~ r => StringPointsTo(l, Seq(r))
  }

  def parsePointsTo : Parser[StringPointsTo] = parsePtr ~ ("->" ~> (parsePtrSeqInParens | parsePtrSeq)) ^^ {
    case l ~ r => StringPointsTo(l, r)
  }

  def parsePureAtom : Parser[StringPureAtom] = parseEq | parseNEq | "true" ^^ {_ => StringTrue() }

  def parseEq : Parser[StringPureAtom] = parsePtr ~ ("=" ~> parsePtr) ^^ {
    case l ~ r => StringPtrEq(l, r)
  }

  def parseNEq : Parser[StringPureAtom] = parsePtr ~ ("!=" ~> parsePtr) ^^ {
    case l ~ r => StringPtrNEq(l, r)
  }

  def parsePtrSeqInParens : Parser[Seq[StringPtrExpr]] = "(" ~> repsep(parsePtr, ",") <~ ")"

  def parsePtrSeq : Parser[Seq[StringPtrExpr]] = rep1sep(parsePtr, ",")

  def parsePtr : Parser[StringPtrExpr] = "nil" ^^ {_ => StringNullPtr()} | "null" ^^ {_ => StringNullPtr()} | ident ^^ StringPtrVar

  override def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_']*""".r

  def stripCommentLines(input : String, commentPrefix : String) = {
    val lines = input.split("\n")
    val strippedLines = lines.filterNot(_.startsWith(commentPrefix))
    strippedLines.mkString("\n")
  }

}
