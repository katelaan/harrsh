package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

import scala.util.Try

case class PureConstraintTracker private(ensured: Set[PureAtom], missing: Set[PureAtom]) {

  assert((ensured intersect missing).isEmpty,
    s"Overlap between ensured (dis)equalities $ensured and missing (dis)equalities $missing")
  assert(ensured forall (_.isOrdered), s"Unordered constraints in $ensured")
  assert(missing forall (_.isOrdered), s"Unordered constraints in $missing")

  lazy val isConsistent: Boolean = ensured.forall(_.isConsistent) && missing.forall(_.isConsistent)

  def compose(other: PureConstraintTracker): PureConstraintTracker = {
    val combinedEnsured = Closure.ofAtoms(ensured ++ other.ensured).asSetOfAtoms
    val combinedMissing = (missing ++ other.missing) -- combinedEnsured
    PureConstraintTracker(combinedEnsured, combinedMissing)
  }

  def dropVars(vars: Set[Var]): PureConstraintTracker = {
    val containsBound = (a: PureAtom) => vars.contains(a.l) || vars.contains(a.r)
    new PureConstraintTracker(ensured.filterNot(containsBound), missing.filterNot(containsBound))
  }

  def update(f: SubstitutionUpdate): PureConstraintTracker = {
    def updateSet(set: Set[PureAtom]) = for {
      PureAtom(l, r, isEquality) <- set
      fl <- Try{ f(l) }.getOrElse(Set(l))
      fr <- Try{ f(r) }.getOrElse(Set(r))
    } yield PureAtom(fl, fr, isEquality)

    val ensuredUpdated = updateSet(ensured).map(_.ordered)
    val missingUpdated = updateSet(missing).map(_.ordered) -- ensuredUpdated

    new PureConstraintTracker(ensuredUpdated, missingUpdated)
  }

}

object PureConstraintTracker {

  def empty: PureConstraintTracker = apply(Set.empty, Set.empty)

  def apply(ensured: Set[PureAtom], missing: Set[PureAtom]): PureConstraintTracker = {
    new PureConstraintTracker(ensured.map(_.ordered).filterNot(_.isTautology), missing.map(_.ordered).filterNot(_.isTautology))
  }

}
