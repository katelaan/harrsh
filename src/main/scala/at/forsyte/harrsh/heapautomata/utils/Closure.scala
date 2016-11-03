package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jkatelaa on 10/17/16.
  */
trait Closure {

  def asSetOfAtoms : Set[PureAtom]

  def getEqualityClass(fv : Var) : Set[Var]

  def isMinimumInItsClass(fv : Var) : Boolean

}
