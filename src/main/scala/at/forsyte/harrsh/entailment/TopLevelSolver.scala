package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.TopLevelSolver.{InstantiationForLhs, LhsHasNondecomposableProfile, TopLevelPreprocessingResult, LhsIsUnsatisfiable}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RichSid}

trait TopLevelSolver extends HarrshLogging {

  def checkValidity(entailmentInstance: EntailmentInstance, reachable: Map[String, Set[EntailmentProfile]], exportToLatex: Boolean): Boolean = {
    TopLevelSolver.applyIfInstantiation(
      checkValidityOfInstantiation(entailmentInstance, _, _, exportToLatex),
      computeLhsProfilesForComposition(reachable, entailmentInstance)
    )
  }

  def checkValidityOfInstantiation(entailmentInstance: EntailmentInstance, maybeLhsPureProfile: Option[EntailmentProfile], renamedCallProfileSets: Seq[Set[EntailmentProfile]], exportToLatex: Boolean): Boolean

  private def pureProfile(atoms: Seq[PureAtom], computeEvenIfEmpty: Boolean): Option[EntailmentProfile] = {
    if (!computeEvenIfEmpty && atoms.isEmpty) None
    else {
      val vars = atoms.toSet[PureAtom].flatMap(_.getVars)
      val constraints = VarConstraints.fromAtoms(vars, atoms)
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
      reachableForCall = reachable.getOrElse(call.name, Set(ProfileOfNondecomposableModels(Var.getFvSeq(call.args.length).toSet)))
      _ = if (reachableForCall.isEmpty) throw new Exception(""+call)
      // We do not make the ALL-SAT assumption for the top-level formula. Instantiating profiles with the call args
      // can thus yield an inconsistent profile. We discard such profiles here
    } yield reachableForCall flatMap (_.renameOrFail(sid, call.args))
  }

  private def computeLhsProfiles(reachable: Map[String, Set[EntailmentProfile]], entailmentInstance: EntailmentInstance): (Option[EntailmentProfile], Seq[Set[EntailmentProfile]]) = {
    val sid = entailmentInstance.rhs.sid
    val lhsConstraint = entailmentInstance.lhs.topLevelConstraint
    val rhsConstraint = entailmentInstance.rhs.topLevelConstraint
    logger.debug(s"Will check whether all profiles in fixed point for $lhsConstraint imply $rhsConstraint wrt $sid")
    if (!rhsConstraint.isQuantifierFree) throw new IllegalArgumentException("RHS is quantified")
    val renamedCallProfiles = renameForCalls(lhsConstraint, reachable, sid)
    val feasibleCallProfiles = restrictToFeasible(renamedCallProfiles, lhsConstraint)
    // If the LHS does not contain *any* calls, we must always compute a pure profile:
    // Otherwise, we would get the empty profile for the LHS emp and would thus erroneously conclude that
    // there is no consistent profile for emp
    val profileForLhsPureConstraints = pureProfile(lhsConstraint.pure, computeEvenIfEmpty = lhsConstraint.calls.isEmpty)
    logger.debug("Instantiated and filtered profiles are:\n" + FixedPointSerializer(entailmentInstance, markFinalProfiles = true)(profileForLhsPureConstraints.toSet +: feasibleCallProfiles))
    (profileForLhsPureConstraints, feasibleCallProfiles)
  }

  private def restrictToFeasible(renamedCallProfiles: Seq[Set[EntailmentProfile]], lhsConstraint: TopLevelConstraint): Seq[Set[EntailmentProfile]] = {
    renamedCallProfiles map (restrictToFeasible(_, lhsConstraint))
  }

  private def restrictToFeasible(renamedCallProfiles: Set[EntailmentProfile], lhsConstraint: TopLevelConstraint): Set[EntailmentProfile] = {
    renamedCallProfiles filter (isFeasible(_, lhsConstraint))
  }

  private def isFeasible(profile: EntailmentProfile, lhsConstraint: TopLevelConstraint): Boolean = {
    val res = VarConstraints.areCompatible(profile.sharedConstraints, lhsConstraint.pure)
    if (!res) logger.debug(s"Will drop profile $profile, as it is contradicted by ${lhsConstraint.pure}")
    res
  }

  private def allDecomposable(profiless: Seq[Set[EntailmentProfile]]): Boolean = {
    profiless.forall{
      profiles => profiles.nonEmpty && profiles.forall(_.isDecomposable)
    }
  }

  def existsUnsatisfiable(profiless: Seq[Set[EntailmentProfile]]): Boolean = {
    profiless.exists(_.isEmpty)
  }

  def computeLhsProfilesForComposition(reachable: Map[String, Set[EntailmentProfile]], entailmentInstance: EntailmentInstance): TopLevelPreprocessingResult = {
    val (maybeLhsPureProfile, renamedCallProfileSets) = computeLhsProfiles(reachable, entailmentInstance)
    if (existsUnsatisfiable(renamedCallProfileSets)) {
      logger.debug("LHS is unsatisfiable => Entailment holds")
      LhsIsUnsatisfiable
    } else if (allDecomposable(renamedCallProfileSets)) {
      InstantiationForLhs(maybeLhsPureProfile, renamedCallProfileSets)
    } else {
      logger.debug("Entailment does not hold: LHS calls have non-decomposable profile.")
      LhsHasNondecomposableProfile
    }
  }

}

object TopLevelSolver {

  sealed trait TopLevelPreprocessingResult
  case object LhsHasNondecomposableProfile extends TopLevelPreprocessingResult
  case object LhsIsUnsatisfiable extends TopLevelPreprocessingResult
  case class InstantiationForLhs(maybeLhsPureProfile: Option[EntailmentProfile], renamedCallProfileSets: Seq[Set[EntailmentProfile]]) extends TopLevelPreprocessingResult

  def applyIfInstantiation(f: (Option[EntailmentProfile], Seq[Set[EntailmentProfile]]) => Boolean, preprocessingResult: TopLevelPreprocessingResult): Boolean = {
    preprocessingResult match {
      case InstantiationForLhs(maybePureProfile, renamedProfiles) => f(maybePureProfile, renamedProfiles)
      case LhsIsUnsatisfiable => true
      case LhsHasNondecomposableProfile => false
    }
  }



}