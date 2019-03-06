package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentProfile.ForgetFilter
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.RichSid

// TODO: Turn parameters into Set. This probably has repercussions for the current treatment of non-standard calls, so I can't do it right now. See also TODO in ComposeProfiles
case class EntailmentProfile(decomps: Set[ContextDecomposition], orderedParams: Seq[Var]) extends HarrshLogging {

  val nonEmpty = decomps.nonEmpty

  private val nonPlaceholderVarsInContexts = decomps.flatMap(_.nonNullNonPlaceholderVars)

  if (nonEmpty && !(nonPlaceholderVarsInContexts subsetOf orderedParams.toSet)) {
    throw new IllegalArgumentException(s"Profile contains FVs $nonPlaceholderVarsInContexts, but constructing state for $orderedParams")
  }

  def rename(sid: RichSid, to: Seq[Var]): EntailmentProfile = {
    assert(to.size == orderedParams.size)
    val callUpdate: SubstitutionUpdate = v => {
      // If v is the i-th free variable of the predicate, replace it with the i-th argument of the call;
      // otherwise, return the variable as is
      orderedParams.indexOf(v) match {
        case fvIx if fvIx >= 0 => Set(to(fvIx))
        case _ => Set(v)
      }
    }
    val renamed = decomps.map(_.updateSubst(callUpdate))
    val consistent = renamed filterNot (_.isInconsistent(sid))
    EntailmentProfile(consistent, to)
  }

  def forget(varsToForget: Set[Var], filter: ForgetFilter = EntailmentProfile.KeepIfNamesForAllUsedParams): EntailmentProfile = {
    val filteredDecomps = for {
      decomp <- decomps
      restrictedToFreeVars <- decomp.forget(varsToForget)
      _ = logger.debug(s"After restriction to free variables:\n${restrictedToFreeVars.parts.mkString("\n")}")
      _ = assert(restrictedToFreeVars.boundVars.isEmpty, s"Bound vars remain after restriction to free vars: $restrictedToFreeVars")
      // Only keep decompositions that still have enough names. See more detailed comments on the specific filters.
      if filter(restrictedToFreeVars)
      _ = logger.debug("Decomposition has names for all used vars. Will keep if viable.")
    } yield restrictedToFreeVars

    EntailmentProfile(filteredDecomps, orderedParams filterNot varsToForget.contains)
  }

  def dropNonViableDecompositions(sid: RichSid): EntailmentProfile = {
    EntailmentProfile(decomps filter (_.isViable(sid)), orderedParams)
  }

}

object EntailmentProfile {

  type ForgetFilter = ContextDecomposition => Boolean

  /**
    * At the end of the composition, every used parameter needs to have a name, because without names for used params,
    * we can never extend the decomposition to a full unfolding: There's no way to compose if you don't have names.
    * In the TACAS paper, forget simply throws out all decompositions that use the variables we forget. This is not
    * sound here, however, because of possible aliasing effects imposed via pure formulas.
    * Checking if everything that's used has a name *after* the forget operation takes care of this by only discarding
    * decompositions where some variable we forgot was the *only* name for a used parameter.
    */
  val KeepIfNamesForAllUsedParams: ForgetFilter = _.hasNamesForAllUsedParams

  /**
    * It's also possible to discard just profiles with wrong roots, which is still a very effective filter for most SIDs,
    * but yields to significantly more decomps (and thus a performance penalty) e.g. for grids.
    *
    * I keep this as failsafe code for the time being, in case we want to have another look at the structure of the larger
    * profiles or look at the performance on simple SIDs when usingwith this simpler filter.
    */
  def keepIfNonNullNamesForAllRootParams(sid: RichSid): ForgetFilter = _.hasNonNullNamesForAllRootParams(sid)

}

