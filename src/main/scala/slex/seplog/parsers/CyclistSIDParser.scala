package slex.seplog.parsers

import slex.heapautomata._
import slex.seplog.MapBasedRenaming
import slex.seplog.inductive._

/**
  * Created by jkatelaa on 10/20/16.
  */
object CyclistSIDParser extends SIDParser {

  type Rule = (String, Seq[String], SymbolicHeap)
  type PredSpec = (String, Int, Seq[(String, SymbolicHeap)])

  def run(input : String) : Option[(SID,Int)] = {
    val inputWithoutComments = stripCommentLines(input, "#")
    parseAll(parseSID, inputWithoutComments) match {
      case Success(result, next) => Some(result)
      case Failure(msg,_) => println("FAILURE: " + msg); None
      case Error(msg,_) => println("ERROR: " + msg); None
    }
  }

  def parseSID : Parser[(SID,Int)] = rep1sep(parsePredSpec, ";") ^^ {
    case preds =>
      val startPred : String = preds.head._1
      val maxNumFV : Int = preds.map(_._2).max
      val desc : String = startPred + "-SID"
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

      // Rename free vars to xi
      val renamingMap : Map[String,String] = Map() ++ (head._2 zip (1 to head._2.size).map(i => fv(i).toString))
      val bodyWithRenamedVs = bodyWithQs.renameVars(MapBasedRenaming(renamingMap))

      (head._1, head._2 map renamingMap, bodyWithRenamedVs)
  }

  def parseHead = parseHeadWithArgs | parseHeadWithoutArgs

  def parseHeadWithoutArgs : Parser[(String, Seq[String])] = ident ^^ (s => (s,Seq()))

  def parseHeadWithArgs : Parser[(String, Seq[String])] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => (name, args)
  }

  def parseBody : Parser[SymbolicHeap] = parseAtomSeq map {
    atoms =>
      val spatial = atoms filter (_.isInstanceOf[SpatialAtom]) map (_.asInstanceOf[SpatialAtom])
      val pure = atoms filter (_.isInstanceOf[PureAtom]) map (_.asInstanceOf[PureAtom])
      SymbolicHeap(pure, spatial, Seq())
  }

  def parseAtomSeq : Parser[Seq[SepLogAtom]] = rep1sep(parseAtom, "*")

}
