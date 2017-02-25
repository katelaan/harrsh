package at.forsyte.harrsh.seplog.parsers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 10/20/16.
  */
object DefaultSIDParser extends SIDParser with HarrshLogging {

  def run(input : String) : Option[(SID,Int)] = {
    val inputWithoutComments = ParseUtils.stripCommentLines(input, "#")
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

  // TODO This is still somewhat brittle, in that the parser does not detect if the largest free variable of this rule is less than the max free var for other rules of the same predicate, thus erroneously assuming an arity that is too low
  def parseRule : Parser[Rule] = parseHead ~ ("<=" ~> parseBody) ^^ {
    case head ~ body =>
      val (freeVarsUnsorted,boundVarsUnsorted) = body.getVars.toSeq.partition(isFV)
      val (freeVars,boundVars) = (freeVarsUnsorted.sorted, boundVarsUnsorted.sorted)
      val filledFreeVars : Seq[String] = (1 to (freeVars.map(stringToFV).max)) map toDefaultString

      val naming : VarUnNaming = mkUnNaming(filledFreeVars,boundVars) //mkUnNamingFromIncompleteDefaultNames(freeVars, boundVars)
      val renamedBody = body.replaceStringsByIds(naming)
      logger.debug("Assembling rule out of head " + head + " and body " + body + " yielding modified body " + renamedBody)
      Rule(head, filledFreeVars, boundVars, renamedBody)
  }

  def parseHead : Parser[String] = ident

  def parseBody : Parser[StringSymbolicHeap] = parseSpatial ~ opt(":" ~> parsePure) ^^ {
    case spatial ~ pure => StringSymbolicHeap(pure.getOrElse(Seq()), spatial)
  }

  def parseSpatial = rep1sep(parseSpatialAtom, "*")

  def parsePure = "{" ~> rep1sep(parsePureAtom, ",") <~ "}"

}
