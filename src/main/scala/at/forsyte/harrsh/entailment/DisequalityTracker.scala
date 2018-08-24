package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

import scala.util.Try

case class DisequalityTracker private(ensured: Set[PureAtom], missing: Set[PureAtom]) {

  assert((ensured intersect missing).isEmpty,
    s"Overlap between ensured disequalities $ensured and missing disequalities $missing")
  assert(ensured forall (!_.isEquality))
  assert(ensured forall (_.isOrdered), s"Unordered constraints in $ensured")
  assert(missing forall (!_.isEquality))
  assert(missing forall (_.isOrdered), s"Unordered constraints in $missing")

  lazy val isConsistent: Boolean = ensured.forall(_.isConsistent) && missing.forall(_.isConsistent)

  def compose(other: DisequalityTracker): DisequalityTracker = {
    val combinedEnsured = Closure.ofAtoms(ensured ++ other.ensured).asSetOfAtoms
    val combinedMissing = (missing ++ other.missing) -- combinedEnsured
    DisequalityTracker(combinedEnsured, combinedMissing)
  }

  def dropVars(vars: Set[Var]): DisequalityTracker = {
    val containsBound = (a: PureAtom) => vars.contains(a.l) || vars.contains(a.r)
    new DisequalityTracker(ensured.filterNot(containsBound), missing.filterNot(containsBound))
  }

  def update(f: SubstitutionUpdate): DisequalityTracker = {
    def updateSet(set: Set[PureAtom]) = for {
      PureAtom(l,r,_) <- set
      fl <- Try{ f(l) }.getOrElse(Set(l))
      fr <- Try{ f(r) }.getOrElse(Set(r))
    } yield PureAtom(fl, fr, isEquality = false)

    val ensuredUpdated = updateSet(ensured).map(_.ordered)
    val missingUpdated = updateSet(missing).map(_.ordered) -- ensuredUpdated

    new DisequalityTracker(ensuredUpdated, missingUpdated)
  }

}

object DisequalityTracker {

  def empty: DisequalityTracker = apply(Set.empty, Set.empty)

  def apply(ensured: Set[PureAtom], missing: Set[PureAtom]): DisequalityTracker = {
    new DisequalityTracker(ensured.map(_.ordered), missing.map(_.ordered))
  }

}
