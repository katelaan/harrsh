package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.main.Var
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PtrNEq, PureAtom}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/17/16.
  */
object EqualityUtils {

  def propagateConstraints(alloc : Set[Var], pure : Set[PureAtom]) : (Set[Var], Set[PureAtom]) = {

    val allPure = propagateConstraints(pure)
    // Propagate equalities to allocation info
    val allocFromEqualities : Set[Var] = propagateEqualitiesToAlloc(alloc, allPure)

    (allocFromEqualities, allPure)
  }

  @tailrec
  def propagateConstraints(from : Set[PureAtom]): Set[PureAtom] = {
    // TODO This function is inefficient

    val newEqs : Seq[PureAtom] = Combinators.square(from.toIndexedSeq) map {
      case (l, r) => transitiveConstraint(l, r)
    } filter (_.isDefined) map (_.get)

    val combined = from ++ newEqs
    if (combined == from) from else propagateConstraints(combined)

  }

  private def transitiveConstraint(fvA : PureAtom, fvB : PureAtom) : Option[PureAtom] = {

    val (leftA, rightA, isEqualA) = unwrapAtom(fvA)
    val (leftB, rightB, isEqualB) = unwrapAtom(fvB)

    if (isEqualA || isEqualB) {
      // If at least one is an equality, and one end of the eqs coincides, we can propagate
      val newPair: Option[(Var, Var)] =
      if (leftA == leftB) Some((rightA, rightB))
      else if (leftA == rightB) Some((rightA, leftB))
      else if (rightA == leftB) Some((leftA, rightB))
      else if (rightA == rightB) Some((leftA, leftB))
      else None

      newPair map (p => orderedAtom(p._1, p._2, isEqualA && isEqualB))
    }
    else {
      // Can't infer anything if both are inequalities
      None
    }

  }

//  def propagateEqualitiesToAlloc(explicit: Set[FV], allPure: Set[PureAtom]): Set[FV] = {
//    val propagated : Set[FV] = (for {
//      atom <- allPure
//      (l, r, isEq) = unwrapAtom(atom)
//      if isEq
//      if explicit.contains(l) || explicit.contains(r)
//    } yield Set(l,r)).flatten
//
//    explicit union propagated
//  }

  def propagateEqualitiesToAlloc(explicit: Set[Var], allPure: Set[PureAtom]): Set[Var] = {
    propagateEqualitiesToAllocAux(explicit, allPure.filter(_.isInstanceOf[PtrEq]).map(_.asInstanceOf[PtrEq]).toSeq, explicit)
  }

  @tailrec
  private def propagateEqualitiesToAllocAux(explicit: Set[Var], allPure: Seq[PtrEq], acc : Set[Var]): Set[Var] = {
    if (allPure.isEmpty) acc else {
      val hd = allPure.head
      val (l, r) = (hd.l, hd.r)
      val newAcc = if (explicit.contains(l.getVarOrZero)) acc + r.getVarOrZero else if(explicit.contains(r.getVarOrZero)) acc + l.getVarOrZero else acc
      propagateEqualitiesToAllocAux(explicit, allPure.tail, newAcc)
    }
  }

}
