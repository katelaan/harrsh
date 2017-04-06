package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jkatelaa on 10/20/16.
  */
private[parsers] trait SIDCombinatorParser extends JavaTokenParsers with SIDParser {

  val printFailure : Boolean = true

  def parseSID : Parser[SID]

  def parseBody : Parser[StringSymbolicHeap]

  override final def runOnSID(input : String) : Option[SID] = runParser(parseSID)(input)

  override final def runOnSymbolicHeap(input : String) : Option[SymbolicHeap] = runParser(parseBody)(input) map (_.toSymbolicHeap._1)

  override def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_']*""".r

  protected def runParser[A](parser : Parser[A])(input : String) : Option[A] = {
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
