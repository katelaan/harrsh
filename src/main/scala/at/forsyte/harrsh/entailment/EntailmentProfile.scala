package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.Var

case class EntailmentProfile(profile: Set[ContextDecomposition], orderedParams: Seq[Var]) {
  
  val nonEmpty = profile.nonEmpty

  private val nonPlaceholderVarsInContexts = profile.flatMap(_.nonNullNonPlaceholderVars)

  if (nonEmpty && !(nonPlaceholderVarsInContexts subsetOf orderedParams.toSet)) {
    throw new IllegalArgumentException(s"Profile contains FVs $nonPlaceholderVarsInContexts, but constructing state for $orderedParams")
  }

  def compose(other: EntailmentProfile): EntailmentProfile = ???

  def rename(to: Seq[Var]): EntailmentProfile = {
    assert(to.size == orderedParams.size)
    val callUpdate: SubstitutionUpdate = v => {
      // If v is the i-th free variable of the predicate, replace it with the i-th argument of the call;
      // otherwise, return the variable as is
      orderedParams.indexOf(v) match {
        case fvIx if fvIx >= 0 => Set(to(fvIx))
        case _ => Set(v)
      }
    }
    val renamed = profile.map(_.updateSubst(callUpdate))
    val consistent = renamed filterNot (_.isInconsistent)
    EntailmentProfile(consistent, to)
  }

  def forget(vs: Seq[Var]): Option[EntailmentProfile] = ???

}
