package slex.slex

import slex.heapautomata._
import slex.seplog.PureAtom

/**
  * Created by jkatelaa on 10/17/16.
  */
package object hepautomata {

  def mkPure(atoms : (Int, Int, Boolean)*) : Set[PureAtom] = Set() ++ (atoms.toSeq map {
    case (l,r,isEq) => orderedAtom(fv(l),fv(r),isEq)
  })

  def fvAll(ints : Int*) : Set[FV] = Set() ++ ints map fv

}
