package at.forsyte.harrsh.seplog.parsers

import at.forsyte.harrsh.seplog.mkUnNaming
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 10/20/16.
  */
object CyclistSIDParser extends SIDParser {

  type PredSpec = (String, Int, Seq[Rule])

  def run(input : String) : Option[(SID,Int)] = {
    val inputWithoutComments = ParseUtils.stripCommentLines(input, "#")
    parseAll(parseSID, inputWithoutComments) match {
      case Success(result, next) => Some(result)
      case Failure(msg,_) => println("FAILURE: " + msg); None
      case Error(msg,_) => println("ERROR: " + msg); None
    }
  }

  def parseSID : Parser[(SID,Int)] = rep1sep(parsePredSpec, ";") ^^ {
    preds =>
      val startPred : String = preds.head._1
      val maxNumFV : Int = preds.map(_._2).max
      val desc : String = startPred + "-SID"
      val allRules : Seq[Rule] = preds.flatMap(_._3)
      (new SID(startPred, allRules.toSet, desc), maxNumFV)
  }

  def parsePredSpec : Parser[PredSpec] = ident ~ ("{" ~> parseRuleSeq <~ "}") ^^ {
    case name ~ rules => (name, rules.head.freeVars.size, rules)
  }

  def parseRuleSeq : Parser[Seq[Rule]] = rep1sep(parseRule, "|")

  def parseRule : Parser[Rule] = parseBody ~ ("=>" ~> parseHead) ^^ {
    case body ~ head =>
      val (headPred, freeVars) = head
      val boundVars = (body.getVars -- freeVars).toSeq

      Rule(headPred, freeVars, boundVars, body.replaceStringsByIds(mkUnNaming(freeVars, boundVars)))
  }

  def parseHead = parseHeadWithArgs | parseHeadWithoutArgs

  def parseHeadWithoutArgs : Parser[(String, Seq[String])] = ident ^^ (s => (s,Seq()))

  def parseHeadWithArgs : Parser[(String, Seq[String])] = ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ {
    case name ~ args => (name, args)
  }

  def parseBody : Parser[StringSymbolicHeap] = parseAtomSeq map {
    atoms =>
      val spatial = atoms filter (_.isInstanceOf[StringSpatialAtom]) map (_.asInstanceOf[StringSpatialAtom])
      val pure = atoms filter (_.isInstanceOf[StringPureAtom]) map (_.asInstanceOf[StringPureAtom])
      StringSymbolicHeap(pure, spatial)
  }

  def parseAtomSeq : Parser[Seq[StringSepLogAtom]] = rep1sep(parseAtom, "*")

}
