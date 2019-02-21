package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{Sid, SymbolicHeap}
import at.forsyte.harrsh.util.IOUtils

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jkatelaa on 10/20/16.
  */
private[parsers] trait SidCombinatorParser extends JavaTokenParsers with SidParser {

  def parseSID : Parser[Sid]

  def parseBody : Parser[StringSymbolicHeap]

  def parseSymbolicHeap : Parser[SymbolicHeap]

  override final def runOnSid(input : String, printFailure : Boolean = true) : Option[Sid] = catchNumberFormatException{
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
