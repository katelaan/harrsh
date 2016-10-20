package slex.seplog.parsers

import slex.heapautomata._
import slex.seplog.{MapBasedRenaming, PtrExpr, PtrVar, Renaming}
import slex.seplog.inductive._

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SIDParser extends JavaTokenParsers {

  def parseAtom : Parser[SepLogAtom] = parseSpatialAtom | parsePureAtom

  def parseSpatialAtom : Parser[SpatialAtom] = parsePointsTo | parseCall | parseEmp

  def parseEmp : Parser[Emp] = "emp" ^^ { _ => Emp() }

  def parseCall : Parser[PredCall] = ident ~ parsePtrSeqInParens ^^ {
    case name ~ args => PredCall(name, args)
  }

  def parsePointsSingleTo : Parser[PointsTo] = parsePtr ~ ("->" ~> parsePtr) ^^ {
    case l ~ r => ptr(l, r)
  }

  def parsePointsTo : Parser[PointsTo] = parsePtr ~ ("->" ~> (parsePtrSeqInParens | parsePtrSeq)) ^^ {
    case l ~ r => PointsTo(l, r)
  }

  def parsePureAtom : Parser[PureAtom] = parseEq | parseNEq | "true" ^^ {_ => True() }

  def parseEq : Parser[PureAtom] = parsePtr ~ ("=" ~> parsePtr) ^^ {
    case l ~ r => ptreq(l, r)
  }

  def parseNEq : Parser[PureAtom] = parsePtr ~ ("!=" ~> parsePtr) ^^ {
    case l ~ r => ptrneq(l, r)
  }

  def parsePtrSeqInParens : Parser[Seq[PtrExpr]] = "(" ~> rep1sep(parsePtr, ",") <~ ")"

  def parsePtrSeq : Parser[Seq[PtrExpr]] = rep1sep(parsePtr, ",")

  def parsePtr : Parser[PtrExpr] = "nil" ^^ {_ => nil} | "null" ^^ {_ => nil} | ident ^^ PtrVar

  override def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_']*""".r

  def renameFVs(fvs : Seq[String], sh : SymbolicHeap) : (SymbolicHeap, Map[String,String]) = {
    val renamingMap : Map[String,String] = Map() ++ (fvs zip (1 to fvs.size).map(i => fv(i).toString))
    (sh.renameVars(MapBasedRenaming(renamingMap)), renamingMap)
  }

  def stripCommentLines(input : String, commentPrefix : String) = {
    val lines = input.split("\n")
    val strippedLines = lines.filterNot(_.startsWith(commentPrefix))
    strippedLines.mkString("\n")
  }

}
