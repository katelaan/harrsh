package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
import at.forsyte.harrsh.parsers.buildingblocks.{AsciiAtoms, EmptyQuantifierPrefix, UnicodeAtoms, UnicodeQuantifierPrefix}

import scala.annotation.tailrec

/**
  * Created by jens on 4/6/17.
  */
object SIDParsers {

  val DefaultSIDParser : SIDParser = new HarrshSIDParser with AsciiAtoms with EmptyQuantifierPrefix

  val CyclistSIDParser : SIDParser = new CyclistStyleSIDParser with AsciiAtoms

  val PrettyPrintedSIDParser : SIDParser = new HarrshSIDParser with UnicodeAtoms with UnicodeQuantifierPrefix

  val CombinedSIDParser : SIDParser = new SIDParser {

    private val parsers = Seq(DefaultSIDParser, PrettyPrintedSIDParser, CyclistSIDParser)

    override def runOnSID(input: String, printFailure : Boolean = false): Option[SID] = tryAll(parsers, input){
      (parser, input) => parser.runOnSID(input, printFailure)
    }

    override def runOnSymbolicHeap(input: String, printFailure : Boolean = false): Option[SymbolicHeap] = tryAll(parsers, input){
      (parser, input) => parser.runOnSymbolicHeap(input, printFailure)
    }

    /**
      * Apply all of the given parsers on the given input, for each parser running the function f
      */
    @tailrec private def tryAll[A](parsers : Seq[SIDParser], input : String)(f : (SIDParser, String) => Option[A]) : Option[A] = if (parsers.isEmpty) {
      None
    } else {
      f(parsers.head, input) match {
        case success : Some[A] => success
        case None => tryAll(parsers.tail, input)(f)
      }
    }

  }

}
