package at.forsyte.harrsh.parsers.buildingblocks

import at.forsyte.harrsh.parsers.{StringPointsTo, StringPtrEq, StringPtrNEq, StringPureAtom}

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jens on 4/6/17.
  */
trait AsciiAtoms extends Atoms {

  self : JavaTokenParsers =>

  def parsePointsTo : Parser[StringPointsTo] = parsePtr ~ ("->" ~> (parsePtrSeqInParens | parsePtrSeq)) ^^ {
    case l ~ r => StringPointsTo(l, r)
  }

  def parseEq : Parser[StringPureAtom] = parsePtr ~ ("=" ~> parsePtr) ^^ {
    case l ~ r => StringPtrEq(l, r)
  }

  def parseNEq : Parser[StringPureAtom] = parsePtr ~ ("!=" ~> parsePtr) ^^ {
    case l ~ r => StringPtrNEq(l, r)
  }

}
