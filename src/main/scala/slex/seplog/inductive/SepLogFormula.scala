package slex.seplog.inductive

import slex.seplog.Renaming

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SepLogFormula {

    def isSpatial : Boolean

    def isPure : Boolean

    def isSymbolicHeap : Boolean

    def toSymbolicHeap : Option[SymbolicHeap]

    def renameVars(f : Renaming) : SepLogFormula

}
