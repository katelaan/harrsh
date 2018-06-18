package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.modelchecking.{Loc, Model}
import at.forsyte.harrsh.seplog.Var

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Parser for our simple abstract model format
  */
object ModelParser extends JavaTokenParsers {
  def run(input : String) : Option[Model] = {
    val inputWithoutComments = ParseUtils.stripCommentLines(input, "#")
    parseAll(parseModel, inputWithoutComments) match {
      case Success(result, next) => Some(result)
      case Failure(msg,_) => println("FAILURE: " + msg); None
      case Error(msg,_) => println("ERROR: " + msg); None
    }
  }

  def parseModel : Parser[Model] = parseStack ~ (opt(";") ~> parseHeap) ^^ {
    case s ~ h => Model(s,h)
  }

  def parseStack : Parser[Map[Var,Loc]] = "STACK" ~> ("{" ~> parseStackPairs <~ "}") ^^ {
    ls => Map() ++ ls
  }

  def parseHeap : Parser[Map[Loc,Seq[Loc]]] = "HEAP" ~> ("{" ~> parseHeapPairs <~ "}") ^^ {
    ls => Map() ++ ls
  }

  private def parseStackPairs : Parser[List[(Var,Loc)]] = repsep(parseStackPair, ";") <~ opt(";") ^^ {
    ls =>
      // Arbitrarily assign positive numbers to variables
      ls.zipWithIndex map {
        case ((_,l),v) => (Var(v+1),l)
      }
  }

  private def parseStackPair : Parser[(String,Loc)] = ident ~ ("->" ~> wholeNumber) ^^ {
    case v ~ l => (v, Integer.parseInt(l))
  }

  private def parseHeapPairs : Parser[List[(Loc,Seq[Loc])]] = repsep(parsePointer, ";") <~ opt(";")

  private def parsePointer : Parser[(Loc,Seq[Loc])] = wholeNumber ~ ("->" ~> rep1sep(wholeNumber, ",")) ^^ {
    case src ~ trgs => (Integer.parseInt(src), trgs map Integer.parseInt)
  }
}
