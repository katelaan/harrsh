package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{Sid, SymbolicHeap}
import at.forsyte.harrsh.parsers.buildingblocks.{AsciiAtoms, EmptyQuantifierPrefix, UnicodeAtoms, UnicodeQuantifierPrefix}

import scala.annotation.tailrec

/**
  * Created by jens on 4/6/17.
  */
object SidParsers {

  val DefaultSidParser : SidParser = new HarrshSidParser with AsciiAtoms with EmptyQuantifierPrefix

  val CyclistSidParser : SidParser = new CyclistStyleSidParser with AsciiAtoms

  val PrettyPrintedSidParser : SidParser = new HarrshSidParser with UnicodeAtoms with UnicodeQuantifierPrefix

  val CombinedSidParser : SidParser = new SidParser {

    private val parsers = Seq(DefaultSidParser, PrettyPrintedSidParser, CyclistSidParser)

    override def runOnSid(input: String, printFailure : Boolean = false): Option[Sid] = tryAll(parsers, input){
      (parser, input) => parser.runOnSid(input, printFailure)
    }

    override def runOnSymbolicHeap(input: String, printFailure : Boolean = false): Option[SymbolicHeap] = tryAll(parsers, input){
      (parser, input) => parser.runOnSymbolicHeap(input, printFailure)
    }

    /**
      * Apply all of the given parsers on the given input, for each parser running the function f
      */
    @tailrec private def tryAll[A](parsers : Seq[SidParser], input : String)(f : (SidParser, String) => Option[A]) : Option[A] = if (parsers.isEmpty) {
      None
    } else {
      f(parsers.head, input) match {
        case success : Some[A] => success
        case None => tryAll(parsers.tail, input)(f)
      }
    }

  }

}
