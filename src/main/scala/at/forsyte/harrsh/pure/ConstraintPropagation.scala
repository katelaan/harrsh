package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive.{PureAtom}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 5/16/17.
  */
object ConstraintPropagation {

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

    val PureAtom(leftA, rightA, isEqualA) = fvA
    val PureAtom(leftB, rightB, isEqualB) = fvB

    if (isEqualA || isEqualB) {
      // If at least one is an equality, and one end of the eqs coincides, we can propagate
      val newPair: Option[(Var, Var)] =
      if (leftA == leftB) Some((rightA, rightB))
      else if (leftA == rightB) Some((rightA, leftB))
      else if (rightA == leftB) Some((leftA, rightB))
      else if (rightA == rightB) Some((leftA, leftB))
      else None

      newPair map (p => EqualityUtils.orderedAtom(p._1, p._2, isEqualA && isEqualB))
    }
    else {
      // Can't infer anything if both are inequalities
      None
    }

  }

  def propagateEqualitiesToAlloc(explicit: Set[Var], allPure: Set[PureAtom]): Set[Var] = {
    propagateEqualitiesToAllocAux(explicit, allPure.filter(_.isEquality).toSeq, explicit)
  }

  @tailrec
  private def propagateEqualitiesToAllocAux(explicit: Set[Var], allPure: Seq[PureAtom], acc : Set[Var]): Set[Var] = {
    if (allPure.isEmpty) acc else {
      val hd = allPure.head
      val (l, r) = (hd.l, hd.r)
      val newAcc = if (explicit.contains(l)) acc + r else if(explicit.contains(r)) acc + l else acc
      propagateEqualitiesToAllocAux(explicit, allPure.tail, newAcc)
    }
  }

}
