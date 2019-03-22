package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RichSid}

trait TopLevelSolver extends HarrshLogging {

  def checkValidity(sid: RichSid, lhsConstraint: TopLevelConstraint, rhsConstraint: TopLevelConstraint, reachable: Map[String, Set[EntailmentProfile]]): Boolean

  def pureProfile(atoms: Seq[PureAtom], computeEvenIfEmpty: Boolean): Option[EntailmentProfile] = {
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

}
