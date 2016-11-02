package at.forsyte.harrsh.seplog.parsers

import at.forsyte.harrsh.main.FV._
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 10/20/16.
  */
object DefaultSIDParser extends SIDParser {

  def run(input : String) : Option[(SID,Int)] = {
    val inputWithoutComments = stripCommentLines(input, "#")
    parseAll(parseSID, inputWithoutComments) match {
      case Success(result, next) => Some(result)
      case Failure(msg,_) => println("FAILURE: " + msg); None
      case Error(msg,_) => println("ERROR: " + msg); None
    }
  }

  def parseSID : Parser[(SID,Int)] = rep1sep(parseRule, ";") <~ opt(";") ^^ {
    case rules =>
      val startPred : String = rules.head.head
      val maxNumFV : Int = rules.map(_.freeVars.size).max
      val desc : String = startPred + "-SID"
      (new SID(startPred, rules.toSet, desc), maxNumFV)
  }

  def parseRule : Parser[Rule] = parseHead ~ ("<=" ~> parseBody) ^^ {
    case head ~ body =>
      // TODO Do variable ordering in SID parsing?
      val (freeVars,boundVars) = body.getVars.toSeq.partition(isFV)

      val naming : VarUnNaming = mkUnNaming(freeVars, boundVars)

      Rule(head, freeVars, boundVars, body.replaceStringsByIds(naming))
  }

  def parseHead : Parser[String] = ident

  def parseBody : Parser[StringSymbolicHeap] = parseSpatial ~ opt(":" ~> parsePure) ^^ {
    case spatial ~ pure => StringSymbolicHeap(pure.getOrElse(Seq()), spatial)
  }

  def parseSpatial = rep1sep(parseSpatialAtom, "*")

  def parsePure = "{" ~> rep1sep(parsePureAtom, ",") <~ "}"

}
