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

  // TODO: Shouldn't we check the closure for consistency or is there some invariant that guarantees that inconsistency will manifest in one of the parts? Or is it enough that we compute the closure of ensured constraints (as we do already)?
  lazy val isConsistent: Boolean = {
    ensured.forall(_.isConsistent) && missing.forall(_.isConsistent)
  }

  lazy val closure: Closure = Closure.ofAtoms(ensured ++ missing)

  def ++(other: PureConstraintTracker): PureConstraintTracker = {
    val combinedEnsured = Closure.ofAtoms(ensured ++ other.ensured).asSetOfAtoms
    val combinedMissing = (missing ++ other.missing) -- combinedEnsured
    PureConstraintTracker(combinedEnsured, combinedMissing)
  }

  def dropNonFreeVars(vars: Set[Var]): PureConstraintTracker = {
    assert(vars forall (v => v.isBound || PlaceholderVar.isPlaceholder(v)))
    val containsVarToDrop = (a: PureAtom) => vars.contains(a.l) || vars.contains(a.r)
    new PureConstraintTracker(ensured.filterNot(containsVarToDrop), missing.filterNot(containsVarToDrop))
  }

  def restrictPlaceholdersTo(vars: Set[Var]): PureConstraintTracker = {
    val isVarToKeep = (v: Var) => vars(v) || !PlaceholderVar.isPlaceholder(v)
    val containsVarsToKeep = (a: PureAtom) => isVarToKeep(a.l) && isVarToKeep(a.r)
    new PureConstraintTracker(ensured.filter(containsVarsToKeep), missing.filter(containsVarsToKeep))
  }

  def refersOnlyToPlaceholdersIn(vars: Set[Var]): Boolean = {
    val mayOccur = (v: Var) => vars(v) || !PlaceholderVar.isPlaceholder(v)
    val containsOnlyGivenVars = (a: PureAtom) => mayOccur(a.l) && mayOccur(a.r)
    (ensured ++ missing) forall containsOnlyGivenVars
  }

  def update(f: SubstitutionUpdate): PureConstraintTracker = {
    def updateSet(set: Set[PureAtom]) = for {
      PureAtom(l, r, isEquality) <- set
      fl <- Try{ f(l) }.getOrElse(Set(l))
      fr <- Try{ f(r) }.getOrElse(Set(r))
    } yield PureAtom(fl, fr, isEquality)

    val ensuredUpdated = updateSet(ensured).map(_.ordered)
    val missingUpdated = updateSet(missing).map(_.ordered) -- ensuredUpdated

    PureConstraintTracker(ensuredUpdated, missingUpdated)
  }

  def addToMissing(extraMissing: Iterable[PureAtom]): PureConstraintTracker = {
    assert(!extraMissing.exists(ensured),
      s"Trying to add $extraMissing to missing, but ${extraMissing.filter(ensured)} is already ensured")
    PureConstraintTracker(ensured, missing ++ extraMissing)
  }

  def addToMissingUnlessEnsured(extraMissing: Iterable[PureAtom]): PureConstraintTracker = {
    // FIXME Don't we have to compute the closure of the ensured constraints? I think we might.
    val newConstraints = extraMissing map (_.ordered) filterNot ensured
    PureConstraintTracker(ensured, missing ++ newConstraints)
  }

  def dropMissingIfImpliedByAllocation(usageInfo: VarUsageByLabel): PureConstraintTracker = {
    //def usageOfVar(v: Var) = usageInfo.find(_._1.contains(v)).map(_._2).getOrElse(VarUnused)
    def usageOfVar(v: Var) = usageInfo.find(_._1.contains(v)).get._2
    def impliedByAllocation(atom: PureAtom) = {
      println(s"Checking if $atom is implied by $usageInfo")
      !atom.isEquality && usageOfVar(atom.l) == VarAllocated && usageOfVar(atom.r) == VarAllocated
    }
    new PureConstraintTracker(ensured, missing filterNot impliedByAllocation)
  }

}

object PureConstraintTracker {

  def empty: PureConstraintTracker = apply(Set.empty, Set.empty)

  def apply(ensured: Set[PureAtom], missing: Set[PureAtom]): PureConstraintTracker = {
    new PureConstraintTracker(ensured.map(_.ordered).filterNot(_.isTautology), missing.map(_.ordered).filterNot(_.isTautology))
  }

}
