package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.parsers.buildingblocks.Atoms
import at.forsyte.harrsh.seplog.Var.Naming

/**
  * Created by jkatelaa on 10/20/16.
  */
private[parsers] trait CyclistStyleSIDParser extends SIDCombinatorParser {

  self : Atoms =>

  type PredSpec = (String, Seq[RuleBody])

  override def parseSID : Parser[SID] = rep1sep(parsePredSpec, ";") ^^ {
    predSpecs =>
      val startPred : String = predSpecs.head._1
      val desc : String = startPred + "-SID"
      val preds = predSpecs.map(spec => Predicate(spec._1, spec._2))
      SID(startPred, preds, desc)
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

  private def parseRuleSeq : Parser[Seq[RuleBody]] = rep1sep(parseRuleBody, "|")

  private def parseRuleBody : Parser[RuleBody] = parseBody ~ ("=>" ~> parseHead) ^^ {
    case body ~ head =>
      val (_, freeVars) = head // The head predicate is redundant info, so bind it to _
      val boundVars = (body.getVars -- freeVars).toSeq

      RuleBody(boundVars, body.replaceStringsByIds(Naming.mkUnNaming(freeVars, boundVars)).copy(freeVars = freeVars.map(FreeVar)))
  }

  private def parseHead = parseHeadWithArgs | parseHeadWithoutArgs

  private def parseHeadWithoutArgs : Parser[(String, Seq[String])] = ident ^^ (s => (s,Seq()))

  private def parseHeadWithArgs : Parser[(String, Seq[String])] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => (name, args)
  }

  private def parseAtomSeq : Parser[Seq[StringSepLogAtom]] = rep1sep(parseAtom, "*")

}
