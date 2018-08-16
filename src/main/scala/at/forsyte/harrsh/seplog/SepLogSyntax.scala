package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SepLogSyntax extends ToStringWithVarnames {

    def isSpatial : Boolean

    def isPure : Boolean

    def toSymbolicHeap : SymbolicHeap

    def renameVars(f : Renaming) : SepLogSyntax
}
