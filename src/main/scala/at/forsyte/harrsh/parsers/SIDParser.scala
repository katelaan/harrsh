package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jens on 4/6/17.
  */
trait SIDParser {

  def runOnSID(input : String, printFailure : Boolean = true) : Option[SID]

  def runOnSymbolicHeap(input : String, printFailure : Boolean = true) : Option[SymbolicHeap]

}
