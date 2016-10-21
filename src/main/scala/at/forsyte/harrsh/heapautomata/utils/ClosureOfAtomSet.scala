package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog.PtrExpr
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PtrNEq, PureAtom}

/**
  * Created by jkatelaa on 10/17/16.
  */
class ClosureOfAtomSet(pure : Set[PureAtom]) extends Closure {

  // FIXME: This closure class is ridiculously inefficient, having one copy of each equivalence class per member
  var mapToClasses : Map[FV,Set[FV]] = Map()

  for {
    atom <- pure
    (left, right, isEqual) = unwrapAtom(atom)
    if isEqual
  } {
    extendEntry(left, right)
  }

  override def getEqualityClass(fv : FV) : Set[FV] = mapToClasses.getOrElse(fv, Set(fv))

  override def isMinimumInItsClass(fv : FV) : Boolean = {
    // If the EQ class is defined, check if i is the representation = the minimum of that class
    // Otherwise, no equality for i has been set, so i is the unique and hence minimal element, so it is the representation
    if (mapToClasses.isDefinedAt(fv)) {
      mapToClasses(fv).min(Ordering.fromLessThan[PtrExpr]({
        case p  => p._1 < p._2
      })) == fv
    } else {
      true
    }
  }

  private def extendEntry(key : FV, newVal : FV) = {
    val eqClass = if (mapToClasses.isDefinedAt(key)) {
      // Class is already defined, just add the new value
      mapToClasses(key) + newVal
    } else {
      // Key not in any known eq class yet
      // Either have to extend class for val, if known already, or create new class
      if (mapToClasses.isDefinedAt(newVal)) mapToClasses(newVal) + key else Set(key, newVal)
    }

    // Extend entry for all members of the eq class
    for {
      classMember <- eqClass
    } {
      mapToClasses = mapToClasses + (classMember -> eqClass)
    }
  }

  override lazy val asSetOfAtoms: Set[PureAtom] = EqualityUtils.propagateConstraints(pure)

//  override lazy val asSetOfAtoms: Set[PureAtom] = {
//    val classes : Set[Set[FV]] = mapToClasses.values.toSet
//    val eqClosure : Set[PureAtom] = (for {
//      c <- classes
//      l <- c
//      r <- c
//      if l < r
//    } yield PtrEq(l, r)).toSet
//
//    val neqClosure : Set[PureAtom] = for {
//      PtrNEq(l, r) <- pure.filter(_.isInstanceOf[PtrNEq]).map(_.asInstanceOf[PtrNEq])
//      cl = getEqualityClass(l)
//      cr = getEqualityClass(r)
//      nl <- cl
//      rl <- cr
//    } yield if (nl < rl) PtrNEq(nl, rl) else PtrNEq(rl, nl)
//
//    eqClosure ++ neqClosure
//  }

  /*

  private val eqs : Set[PtrEq] = pure.filter(_.isInstanceOf[PtrEq]).map(_.asInstanceOf[PtrEq])
  private val neqs : Set[PtrNEq] = pure.filter(_.isInstanceOf[PtrNEq]).map(_.asInstanceOf[PtrNEq])

  private val representatives : scala.collection.mutable.Map[FV,FV] = scala.collection.mutable.Map.empty
  private val classes : scala.collection.mutable.Map[FV,Set[FV]] = scala.collection.mutable.Map.empty

  for {
    PtrEq(left, right) <- eqs
  } {
    extendEntry(left, right)
  }

  private def extendEntry(l : FV, r : FV) = {
    val (min, max) = if (l < r) (l,r) else (r,l)

    // Merge the classes for min and max
    val repMin = representatives.getOrElse(min,min)
    val repMax = representatives.getOrElse(max,max)
    val maxClass = getEqualityClass(max)
    val minClass = getEqualityClass(min)

    representatives ++= (maxClass map (a => (a, repMin)))
    (classes -= repMax) += (repMin -> minClass.union(maxClass))
  }

  override def getEqualityClass(fv : FV) : Set[FV] = classes.getOrElse(representatives.getOrElse(fv,fv), Set(fv))

  override def isMinimumInItsClass(fv : FV) : Boolean = {
    //println("Representatives: " + representatives.mkString(","))
    //println("Classes: " + classes.mkString(","))
    representatives.getOrElse(fv, fv) == fv
  }

  override lazy val asSetOfAtoms: Set[PureAtom] = {
    val eqs : Set[PureAtom] = (for {
      c <- classes.values
      l <- c
      r <- c
      if l < r
    } yield PtrEq(l, r)).toSet

    val neq : Set[PureAtom] = for {
      PtrNEq(l, r) <- neqs
      cl = getEqualityClass(l)
      cr = getEqualityClass(r)
      nl <- cl
      rl <- cr
    } yield if (nl < rl) PtrNEq(nl, rl) else PtrNEq(rl, nl)

    eqs ++ neqs
  }

   */

}
