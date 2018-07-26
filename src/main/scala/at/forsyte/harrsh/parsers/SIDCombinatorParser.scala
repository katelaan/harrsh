package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
import at.forsyte.harrsh.util.IOUtils

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jkatelaa on 10/20/16.
  */
private[parsers] trait SIDCombinatorParser extends JavaTokenParsers with SIDParser {

  def parseSID : Parser[SID]

  def parseBody : Parser[StringSymbolicHeap]

  override final def runOnSID(input : String, printFailure : Boolean = true) : Option[SID] = catchNumberFormatException{
    runParser(parseSID)(input, printFailure)
  }

  private[parsers] def catchNumberFormatException[A](f : => Option[A]) : Option[A] = {
    try {
      f
    } catch {
      case e : NumberFormatException =>
        IOUtils.printWarningToConsole("Conversion of variables failed -- make sure to use x1,x2,... as free variable identifiers")
        None
    }
  }

  override def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_']*""".r

  protected def runParser[A](parser : Parser[A])(input : String, printFailure : Boolean) : Option[A] = {
    val inputWithoutComments = ParseUtils.stripCommentLines(input, "#")
    parseAll(parser, inputWithoutComments) match {
      case Success(result, next) => Some(result)
      case Failure(msg,_) =>
        if (printFailure) println("FAILURE: " + msg)
        None
      case Error(msg,_) =>
        if (printFailure) println("ERROR: " + msg)
        None
    }
  }

}
