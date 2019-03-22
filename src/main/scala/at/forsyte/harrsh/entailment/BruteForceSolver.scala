package at.forsyte.harrsh.entailment
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.RichSid
import at.forsyte.harrsh.util.Combinators

object BruteForceSolver extends TopLevelSolver {

  override def checkValidity(sid: RichSid, lhsConstraint: TopLevelConstraint, rhsConstraint: TopLevelConstraint, reachable: Map[String, Set[EntailmentProfile]]): Boolean = {

      logger.debug(s"Will check whether all profiles in fixed point for $lhsConstraint imply $rhsConstraint wrt $sid")
      if (!rhsConstraint.isQuantifierFree) throw new IllegalArgumentException("RHS is quantified")
      val renamedReachableStates = for {
        call <- lhsConstraint.calls
        // Note: Because of ALL-SAT of the underlying SID, emptiness of the fixed point means that there is no way to
        // express the predicate wrt the RHS-SID as opposed to that the predicate is UNSAT. For this reason, we add
        // an empty profile in case the fixed point does not contain an entry for the predicate.
        reachableForCall = reachable.getOrElse(call.name, Set(ProfileOfNondecomposableModels(Var.getFvSeq(call.args.length))))
        _ = if (reachableForCall.isEmpty) throw new Exception(""+call)
        // We do not make the ALL-SAT assumption for the top-level formula. Instantiating profiles with the call args
        // can thus yield an inconsistent profile. We discard such profiles here
      } yield reachableForCall flatMap (_.renameOrFail(sid, call.args))
      // If the LHS does not contain *any* calls, we must always compute a pure profile:
      // Otherwise, we would get the empty profile for the LHS emp and would thus erroneously conclude that
      // there is no consistent profile for emp
      val profileForLhsPureConstraints = pureProfile(lhsConstraint.pure, computeEvenIfEmpty = lhsConstraint.calls.isEmpty)
      val combinedProfiles = for {
        toplevelStatesForCalls <- Combinators.choices(renamedReachableStates.map(_.toSeq))
        toplevelStates = profileForLhsPureConstraints match {
          case None => toplevelStatesForCalls
          case Some(pureProfile) => pureProfile +: toplevelStatesForCalls
        }
        // TODO: Is it correct that we don't want/need all composition steps of TargetProfile.composeAndForget? (Since emp closure is done in the acceptance check, and focus check is thus not necessary)
        composed <- EntailmentProfileComposition.composeAll(sid, toplevelStates, lhsConstraint.nonNullVars)
        merged = EntailmentProfileComposition.mergeUsingNonProgressRules(composed, sid)
        restricted = if (lhsConstraint.isQuantifierFree) merged else merged.forget(lhsConstraint.boundVars)
      } yield restricted

      logger.debug(combinedProfiles.size + " combined profile(s):\n" + combinedProfiles.mkString("\n"))

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
