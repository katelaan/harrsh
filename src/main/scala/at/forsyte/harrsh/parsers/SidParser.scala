package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{Sid, SymbolicHeap}

/**
  * Created by jens on 4/6/17.
  */
trait SIDParser {

  def runOnSID(input : String, printFailure : Boolean = true) : Option[Sid]

  def runOnSymbolicHeap(input : String, printFailure : Boolean = true) : Option[SymbolicHeap] = {
    // Parsing of single symbolic heaps not supported by default
    None
  }

}
