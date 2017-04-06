package at.forsyte.harrsh.parsers.buildingblocks

import at.forsyte.harrsh.parsers._

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jens on 4/6/17.
  */
trait Atoms {

  self : JavaTokenParsers =>

  def parseAtom : Parser[StringSepLogAtom] = parseSpatialAtom | parsePureAtom

  def parseSpatialAtom : Parser[StringSpatialAtom] = parsePointsTo | parseCall | parseEmp

  def parseEmp : Parser[StringEmp] = "emp" ^^ { _ => StringEmp() }

  def parseCall : Parser[StringPredCall] = ident ~ parsePtrSeqInParens ^^ {
    case name ~ args => StringPredCall(name, args)
  }

  def parsePtrSeqInParens : Parser[Seq[StringPtrExpr]] = "(" ~> repsep(parsePtr, ",") <~ ")"

  def parsePtrSeq : Parser[Seq[StringPtrExpr]] = rep1sep(parsePtr, ",")

  def parsePtr : Parser[StringPtrExpr] = "nil" ^^ {_ => StringNullPtr()} | "null" ^^ {_ => StringNullPtr()} | ident ^^ StringPtrVar

  def parsePointsSingleTo : Parser[StringPointsTo] = parsePtr ~ ("->" ~> parsePtr) ^^ {
    case l ~ r => StringPointsTo(l, Seq(r))
  }

  def parsePureAtom : Parser[StringPureAtom] = parseEq | parseNEq | "true" ^^ {_ => StringTrue() }

  def parsePointsTo : Parser[StringPointsTo]

  def parseEq : Parser[StringPureAtom]

  def parseNEq : Parser[StringPureAtom]

}
