package at.forsyte.harrsh.entailment

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

  def forget(sid: RichSid, varsToForget: Set[Var]): EntailmentProfile = {
    val filteredDecomps = for {
      decomp <- decomps
      restrictedToFreeVars <- decomp.forget(varsToForget)
      _ = logger.debug(s"After restriction to free variables:\n${restrictedToFreeVars.parts.mkString("\n")}")
      _ = assert(restrictedToFreeVars.boundVars.isEmpty, s"Bound vars remain after restriction to free vars: $restrictedToFreeVars")
      // FIXME: Also filter out types that lack names for back pointers?
      // FIXME: What to check for the top-level predicates that don't have a root parameter annotation?
      // At the end of the composition, every root parameter needs to have a name because without having names for the roots,
      // we can never extend the decomposition to a full unfolding: There's no way to compose if you don't have names.
      // TODO: Instead use the filter operation proposed in the paper?
      if restrictedToFreeVars.hasNamesForAllRootParams(sid)
      _ = logger.debug("Decomposition has names for all roots. Will keep if viable.")
    } yield restrictedToFreeVars

    EntailmentProfile(filteredDecomps, orderedParams filterNot varsToForget.contains)
  }

  def dropNonViableDecompositions(sid: RichSid): EntailmentProfile = {
    EntailmentProfile(decomps filter (_.isViable(sid)), orderedParams)
  }

}