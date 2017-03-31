package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SepLogFormula extends ToStringWithVarnames {

    def isSpatial : Boolean

    def isPure : Boolean

    def isSymbolicHeap : Boolean

    def toSymbolicHeap : Option[SymbolicHeap]

    def renameVars(f : Renaming) : SepLogFormula

}
