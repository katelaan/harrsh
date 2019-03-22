package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RichSid}

trait TopLevelSolver extends HarrshLogging {

  def checkValidity(sid: RichSid, lhsConstraint: TopLevelConstraint, rhsConstraint: TopLevelConstraint, reachable: Map[String, Set[EntailmentProfile]]): Boolean

  private def pureProfile(atoms: Seq[PureAtom], computeEvenIfEmpty: Boolean): Option[EntailmentProfile] = {
    if (!computeEvenIfEmpty && atoms.isEmpty) None
    else {
      val vars = atoms.flatMap(_.getVars).distinct
      val constraints = VarConstraints.fromAtoms(vars.toSet, atoms)
      val decomp = ContextDecomposition(Set.empty, constraints)
      val profile = ProfileOfDecomps(Set(decomp), constraints, vars.filter(!_.isNull))
      logger.debug(s"Created pure profile $profile from top-level atoms $atoms")
      Some(profile)
    }
  }

  private def renameForCalls(constraint: TopLevelConstraint, reachable: Map[String, Set[EntailmentProfile]], sid: RichSid): Seq[Set[EntailmentProfile]] = {
    for {
      call <- constraint.calls
      // Note: Because of ALL-SAT of the underlying SID, emptiness of the fixed point means that there is no way to
      // express the predicate wrt the RHS-SID as opposed to that the predicate is UNSAT. For this reason, we add
      // an empty profile in case the fixed point does not contain an entry for the predicate.
      reachableForCall = reachable.getOrElse(call.name, Set(ProfileOfNondecomposableModels(Var.getFvSeq(call.args.length))))
      _ = if (reachableForCall.isEmpty) throw new Exception(""+call)
      // We do not make the ALL-SAT assumption for the top-level formula. Instantiating profiles with the call args
      // can thus yield an inconsistent profile. We discard such profiles here
    } yield reachableForCall flatMap (_.renameOrFail(sid, call.args))
  }

  def computeLhsProfiles(reachable: Map[String, Set[EntailmentProfile]], lhsConstraint: TopLevelConstraint, rhsConstraint: TopLevelConstraint, sid: RichSid): (Option[EntailmentProfile], Seq[Set[EntailmentProfile]]) = {
    logger.debug(s"Will check whether all profiles in fixed point for $lhsConstraint imply $rhsConstraint wrt $sid")
    if (!rhsConstraint.isQuantifierFree) throw new IllegalArgumentException("RHS is quantified")
    val renamedReachableStates = renameForCalls(lhsConstraint, reachable, sid)
    // If the LHS does not contain *any* calls, we must always compute a pure profile:
    // Otherwise, we would get the empty profile for the LHS emp and would thus erroneously conclude that
    // there is no consistent profile for emp
    val profileForLhsPureConstraints = pureProfile(lhsConstraint.pure, computeEvenIfEmpty = lhsConstraint.calls.isEmpty)
    (profileForLhsPureConstraints, renamedReachableStates)
  }

}
