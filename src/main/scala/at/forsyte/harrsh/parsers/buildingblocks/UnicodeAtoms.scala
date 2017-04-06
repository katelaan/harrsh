package at.forsyte.harrsh.parsers.buildingblocks

import at.forsyte.harrsh.parsers._

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jens on 4/6/17.
  */
trait UnicodeAtoms extends Atoms {

  self : JavaTokenParsers =>

  def parsePointsTo : Parser[StringPointsTo] = parsePtr ~ ("\u21a6" ~> (parsePtrSeqInParens | parsePtrSeq)) ^^ {
    case l ~ r => StringPointsTo(l, r)
  }

  def parseEq : Parser[StringPureAtom] = parsePtr ~ ("\u2248" ~> parsePtr) ^^ {
    case l ~ r => StringPtrEq(l, r)
  }

  def parseNEq : Parser[StringPureAtom] = parsePtr ~ ("\u2249" ~> parsePtr) ^^ {
    case l ~ r => StringPtrNEq(l, r)
  }

}
