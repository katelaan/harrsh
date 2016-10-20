package slex.seplog.parsers

import slex.seplog.{MapBasedRenaming, PtrExpr, PtrVar}
import slex.seplog.inductive._
import slex.heapautomata.fv

import scala.util.parsing.combinator.JavaTokenParsers

// TODO Note: No explicit quantification
// TODO Note: FVs need to be renamed

/**
  * Created by jkatelaa on 10/20/16.
  */
object CyclistSIDParser extends JavaTokenParsers {

  type Rule = (String, Seq[String], SymbolicHeap)
  type PredSpec = (String, Int, Seq[(String, SymbolicHeap)])

  def run(input : String) : Option[(SID,Int)] = parseAll(parseSID, input) match {
    case Success(result, next) => Some(result)
    case Failure(msg,_) => println("FAILURE: " + msg); None
    case Error(msg,_) => println("ERROR: " + msg); None
  }

  def parseSID : Parser[(SID,Int)] = rep1sep(parsePredSpec, ";") ^^ {
    case preds =>
      val startPred : String = preds.head._1
      val maxNumFV : Int = preds.map(_._2).max
      val desc : String = startPred + " (parsed)"
      val allRules : Seq[(String,SymbolicHeap)] = preds.flatMap(_._3)
      (new SID(startPred, allRules.toSet, desc), maxNumFV)
  }

  def parsePredSpec : Parser[PredSpec] = ident ~ ("{" ~> parseRuleSeq <~ "}") ^^ {
    case name ~ rules => (name, rules.head._2.size, rules map (triple => (triple._1, triple._3)))
  }

  def parseRuleSeq : Parser[Seq[Rule]] = rep1sep(parseRule, "|")

  def parseRule : Parser[Rule] = parseBody ~ ("=>" ~> parseHead) ^^ {
    case body ~ head =>
      // Find quantified variables + add quantifiers
      val boundVars = (body.getVars -- head._2).toSeq
      val bodyWithQs = body.copy(qvars = boundVars)

      // Rename free vars to x_i
      val renamingMap : Map[String,String] = Map() ++ (head._2 zip (1 to head._2.size).map(i => fv(i).toString))
      val bodyWithRenamedFVs = bodyWithQs.renameVars(MapBasedRenaming(renamingMap))

      (head._1, head._2 map renamingMap, bodyWithRenamedFVs)
  }

  def parseHead : Parser[(String, Seq[String])] = ident ~ ("(" ~> rep1sep(ident, ",") <~ ")") ^^ {
    case name ~ args => (name, args)
  }

  def parseBody : Parser[SymbolicHeap] = parseAtomSeq map {
    atoms =>
      val spatial = atoms filter (_.isInstanceOf[SpatialAtom]) map (_.asInstanceOf[SpatialAtom])
      val pure = atoms filter (_.isInstanceOf[PureAtom]) map (_.asInstanceOf[PureAtom])
      SymbolicHeap(pure, spatial, Seq())
  }

  def parseAtomSeq : Parser[Seq[SepLogAtom]] = rep1sep(parseAtom, "*")

  def parseAtom : Parser[SepLogAtom] = parseSpatialAtom | parsePureAtom

  def parseSpatialAtom : Parser[SpatialAtom] = parsePointsSingleTo | parsePointsMultipleTo | parseCall | parseEmp

  def parseEmp : Parser[Emp] = "emp" ^^ { _ => Emp() }

  def parseCall : Parser[PredCall] = ident ~ parsePtrSeqInParens ^^ {
    case name ~ args => PredCall(name, args)
  }

  def parsePointsSingleTo : Parser[PointsTo] = parsePtr ~ ("->" ~> parsePtr) ^^ {
    case l ~ r => ptr(l, r)
  }

  def parsePointsMultipleTo : Parser[PointsTo] = parsePtr ~ ("->" ~> parsePtrSeqInParens) ^^ {
    case l ~ r => PointsTo(l, r)
  }

  def parsePureAtom : Parser[PureAtom] = parseEq | parseNEq

  def parseEq : Parser[PureAtom] = parsePtr ~ ("=" ~> parsePtr) ^^ {
    case l ~ r => ptreq(l, r)
  }

  def parseNEq : Parser[PureAtom] = parsePtr ~ ("!=" ~> parsePtr) ^^ {
    case l ~ r => ptrneq(l, r)
  }

  def parsePtrSeqInParens : Parser[Seq[PtrExpr]] = "(" ~> parsePtrSeq <~ ")"

  def parsePtrSeq : Parser[Seq[PtrExpr]] = rep1sep(parsePtr, ",")

  def parsePtr : Parser[PtrExpr] = "nil" ^^ {_ => nil} | ident ^^ PtrVar

  override def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_']*""".r

}
