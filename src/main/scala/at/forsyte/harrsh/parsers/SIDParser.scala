package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jens on 4/6/17.
  */
trait SIDParser {

  def runOnSID(input : String) : Option[SID]

  def runOnSymbolicHeap(input : String) : Option[SymbolicHeap]

}
