package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.{FreeVar, mkUnNaming}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.parsers.buildingblocks.Atoms

/**
  * Created by jkatelaa on 10/20/16.
  */
private[parsers] trait CyclistStyleSIDParser extends SIDCombinatorParser {

  self : Atoms =>

  type PredSpec = (String, Seq[Rule])

  override def parseSID : Parser[SID] = rep1sep(parsePredSpec, ";") ^^ {
    preds =>
      val startPred : String = preds.head._1
      val desc : String = startPred + "-SID"
      val allRules : Seq[Rule] = preds.flatMap(_._2)
      SID(startPred, allRules, desc)
  }

  override def parseBody : Parser[StringSymbolicHeap] = parseAtomSeq map {
    atoms =>
      val spatial = atoms filter (_.isInstanceOf[StringSpatialAtom]) map (_.asInstanceOf[StringSpatialAtom])
      val pure = atoms filter (_.isInstanceOf[StringPureAtom]) map (_.asInstanceOf[StringPureAtom])
      StringSymbolicHeap(pure, spatial)
  }

  private def parsePredSpec : Parser[PredSpec] = ident ~ ("{" ~> parseRuleSeq <~ "}") ^^ {
    case name ~ rules => (name, rules)
  }

  private def parseRuleSeq : Parser[Seq[Rule]] = rep1sep(parseRule, "|")

  private def parseRule : Parser[Rule] = parseBody ~ ("=>" ~> parseHead) ^^ {
    case body ~ head =>
      val (headPred, freeVars) = head
      val boundVars = (body.getVars -- freeVars).toSeq

      Rule(headPred, boundVars, body.replaceStringsByIds(mkUnNaming(freeVars, boundVars)).copy(freeVars = freeVars.map(FreeVar)))
  }

  private def parseHead = parseHeadWithArgs | parseHeadWithoutArgs

  private def parseHeadWithoutArgs : Parser[(String, Seq[String])] = ident ^^ (s => (s,Seq()))

  private def parseHeadWithArgs : Parser[(String, Seq[String])] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => (name, args)
  }

  private def parseAtomSeq : Parser[Seq[StringSepLogAtom]] = rep1sep(parseAtom, "*")

}
