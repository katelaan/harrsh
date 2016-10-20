package slex.seplog.parsers

import slex.heapautomata._
import slex.seplog.PtrVar
import slex.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jkatelaa on 10/20/16.
  */
object DefaultSIDParser extends SIDParser {

  type Rule = (String, SymbolicHeap)

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
      val startPred : String = rules.head._1
      val maxNumFV : Int = rules.map{
        case (head,body) => body.getVars.filter(isFV).map(unFV).max
      }.max
      val desc : String = startPred + "-SID"
      (new SID(startPred, rules.toSet, desc), maxNumFV)
  }

  def parseRule : Parser[Rule] = parseHead ~ ("<=" ~> parseBody) ^^ {
    case head ~ body =>
      // Find quantified variables + add quantifiers
      val boundVars = (body.getVars filterNot (s => isFV(PtrVar(s)))).toSeq
      val bodyWithQs = body.copy(qvars = boundVars)

      (head, bodyWithQs)
  }

  def parseHead : Parser[String] = ident

  def parseBody : Parser[SymbolicHeap] = parseSpatial ~ opt(":" ~> parsePure) ^^ {
    case spatial ~ pure => SymbolicHeap(pure.getOrElse(Seq()), spatial)
  }

  def parseSpatial = rep1sep(parseSpatialAtom, "*")

  def parsePure = "{" ~> rep1sep(parsePureAtom, ",") <~ "}"

}
