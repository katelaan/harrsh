package slex.heapautomata.utils

import slex.heapautomata._
import slex.seplog.inductive.PureAtom

/**
  * Created by jkatelaa on 10/17/16.
  */
trait Closure {

  def asSetOfAtoms : Set[PureAtom]

  def getEqualityClass(fv : FV) : Set[FV]

  def isMinimumInItsClass(fv : FV) : Boolean

}
