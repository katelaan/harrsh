package slex.heapautomata.utils

import slex.Combinators
import slex.heapautomata._
import slex.seplog.PureAtom

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/17/16.
  */
object EqualityUtils {

  def propagateConstraints(alloc : Set[FV], pure : Set[PureAtom]) : (Set[FV], Set[PureAtom]) = {

    val allPure = propagateConstraints(pure)
    // Propagate equalities to allocation info
    val allocFromEqualities : Set[FV] = propagateEqualitiesToAlloc(alloc, allPure)

    (allocFromEqualities, allPure)
  }

  @tailrec
  def propagateConstraints(from : Set[PureAtom]): Set[PureAtom] = {
    // TODO This function is inefficient

    val newEqs : Seq[PureAtom] = (Combinators.square(from.toIndexedSeq) map {
      case (l,r) => transitiveConstraint(l ,r)
    } filter(_.isDefined) map(_.get))

    val combined = from ++ newEqs
    if (combined == from) from else propagateConstraints(combined)

  }

  private def transitiveConstraint(fvA : PureAtom, fvB : PureAtom) : Option[PureAtom] = {

    val (leftA, rightA, isEqualA) = unwrapAtom(fvA)
    val (leftB, rightB, isEqualB) = unwrapAtom(fvB)

    if (isEqualA || isEqualB) {
      // If at least one is an equality, and one end of the eqs coincides, we can propagate
      val newPair: Option[(FV, FV)] =
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

  def propagateEqualitiesToAlloc(explicit: Set[FV], allPure: Set[PureAtom]): Set[FV] = {
    // TODO This is inefficient as well
    val propagated : Set[FV] = (for {
      atom <- allPure
      (l, r, isEq) = unwrapAtom(atom)
      if isEq
      if explicit.contains(l) || explicit.contains(r)
    } yield Set(l,r)).flatten

    explicit union propagated
  }

}
