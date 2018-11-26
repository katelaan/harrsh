package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.FreeVar

case class EntailmentProfile(profile: Set[ContextDecomposition], orderedParams: Seq[FreeVar]) {

  assert(profile forall (_.boundVars.isEmpty),
    s"Trying to construct state from cut profile that still contains bound vars: $profile")

  private val freeVarsInEts = profile.flatMap(_.nonPlaceholderFreeVars)

  if (profile.nonEmpty && !(freeVarsInEts subsetOf orderedParams.toSet)) {
    throw new IllegalArgumentException(s"Cut profile contains FVs $freeVarsInEts, but constructing state for $orderedParams")
  }

}
