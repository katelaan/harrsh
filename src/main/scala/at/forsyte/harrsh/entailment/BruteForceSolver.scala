package at.forsyte.harrsh.entailment
import at.forsyte.harrsh.entailment.EntailmentResultToLatex.{FullTopLevelLatexExporter, NoopLatexExporter, TopLevelSolverLatexExporter}
import at.forsyte.harrsh.seplog.inductive.RichSid
import at.forsyte.harrsh.util.Combinators

object BruteForceSolver extends TopLevelSolver {

  override def checkValidityOfInstantiation(entailmentInstance: EntailmentInstance, profileForLhsPureConstraints: Option[EntailmentProfile], renamedProfiles: Seq[Set[EntailmentProfile]], exportToLatex: Boolean): Boolean = {
    val sid = entailmentInstance.rhs.sid
    val lhsConstraint = entailmentInstance.lhs.topLevelConstraint
    val rhsConstraint = entailmentInstance.rhs.topLevelConstraint
    val exporter = if (exportToLatex) new FullTopLevelLatexExporter(entailmentInstance.rhs.sid, lhsConstraint, rhsConstraint, profileForLhsPureConstraints, renamedProfiles) else NoopLatexExporter
    val combinedProfiles = bruteForceCombinations(sid, lhsConstraint, profileForLhsPureConstraints, renamedProfiles, exporter).toSet
    exporter.writeToLatexFile()
    logger.debug(combinedProfiles.size + " combined profile(s):\n" + combinedProfiles.mkString("\n"))
    checkIfProfilesAreFinal(combinedProfiles, sid, lhsConstraint, rhsConstraint)
  }

  private def bruteForceCombinations(sid: RichSid, lhsConstraint: TopLevelConstraint, profileForLhsPureConstraints: Option[EntailmentProfile], renamedProfiles: Seq[Set[EntailmentProfile]], exporter: TopLevelSolverLatexExporter) = {
    for {
      toplevelProfilesForCalls <- Combinators.choices(renamedProfiles.map(_.toSeq))
      toplevelProfiles  = profileForLhsPureConstraints.toSeq ++ toplevelProfilesForCalls
      // TODO: Is it correct that we don't want/need all composition steps of TargetProfile.composeAndForget? (Since emp closure is done in the acceptance check, and focus check is thus not necessary)
      composed <- EntailmentProfileComposition.composeAndDropVars(toplevelProfiles, lhsConstraint.boundVars, sid, performEmpClosure = false)
      _ = exporter.addComposition(toplevelProfiles, composed)
      _ = logger.debug(s"Merged into $composed")
    } yield composed
  }

  private def checkIfProfilesAreFinal(combinedProfiles: Set[EntailmentProfile], sid: RichSid, lhsConstraint: TopLevelConstraint, rhsConstraint: TopLevelConstraint): Boolean = {
    if (combinedProfiles.isEmpty) {
      logger.info(s"There is no profile for $lhsConstraint => $lhsConstraint is unsatisfiable => entailment holds.")
      true
    } else {
      combinedProfiles.forall { p =>
        logger.debug(s"Will check if $p is final...")
        val res = p.isFinal(sid, rhsConstraint)
        if (!res) logger.debug("Entailment does *not* hold because the following profile is not final:\n" + p)
        res
      }
    }
  }

}
