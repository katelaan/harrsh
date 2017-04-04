package at.forsyte.harrsh.seplog.parsers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 10/20/16.
  */
object DefaultSIDParser extends SIDParser with HarrshLogging {

  def run(input : String) : Option[SID] = {
    val inputWithoutComments = ParseUtils.stripCommentLines(input, "#")
    parseAll(parseSID, inputWithoutComments) match {
      case Success(result, next) => Some(result)
      case Failure(msg,_) => println("FAILURE: " + msg); None
      case Error(msg,_) => println("ERROR: " + msg); None
    }
  }

  def runOnSymbolicHeap(input : String) : Option[SymbolicHeap] = {
    parseAll(parseBody, input) match {
      case Success(result, next) => Some(result.toSymbolicHeap._1)
      case Failure(msg,_) => println("FAILURE: " + msg); None
      case Error(msg,_) => println("ERROR: " + msg); None
    }
  }

  def parseSID : Parser[SID] = rep1sep(parseRule, ";") <~ opt(";") ^^ {
    rules =>
      val startPred : String = rules.head.head
      val maxNumFV : Int = rules.map(_.freeVars.size).max
      val desc : String = startPred + "-SID"
      SID(startPred, rules, desc, maxNumFV)
  }

  // TODO This is still somewhat brittle, in that the parser does not detect if the largest free variable of this rule is less than the max free var for other rules of the same predicate, thus erroneously assuming an arity that is too low
  def parseRule : Parser[Rule] = parseHead ~ ("<=" ~> parseBody) ^^ {
    case head ~ body =>
      val (renamedBody, filledFreeVars, boundVars) = body.toSymbolicHeap
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
