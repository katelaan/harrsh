package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.RichSid
import at.forsyte.harrsh.util.CachedHashcode

sealed trait EntailmentProfile {

  // We only care about ensured constraints here so that we can decide whether the profile is relevant for the top-level formula or not
  assert(!sharedConstraints.isSpeculative)

  val sharedConstraints: VarConstraints
  val orderedParams: Seq[Var]

  def isDecomposable: Boolean = this match {
    case _:ProfileOfNondecomposableModels => false
    case _:ProfileOfDecomps => true
  }

  /**
    * Have a separate definition for this (rather than defining decomps in all deriving classes)
    * so that we can only consciously convert a non-decomposable profile into an empty sequence of decompositions.
    */
  def decompsOrEmptySet: Set[ContextDecomposition]

  def isFinal(sid: RichSid, rhs: TopLevelConstraint): Boolean

  def renameOrFail(sid: RichSid, to: Seq[Var]): Option[EntailmentProfile]

  def forget(varsToForget: Set[Var]): EntailmentProfile

  def isConsistentWithFocus(sid: RichSid): Boolean

  def applyToDecomps(f: Set[ContextDecomposition] => Set[ContextDecomposition]): EntailmentProfile

  def overrideOrderedParams(newParams: Seq[Var]): EntailmentProfile = this match {
    case p:ProfileOfNondecomposableModels => p.copy(orderedParams = newParams)
    case p:ProfileOfDecomps => p.copy(orderedParams = newParams)
  }

}

case class ProfileOfNondecomposableModels(override val sharedConstraints: VarConstraints, override val orderedParams: Seq[Var]) extends EntailmentProfile with CachedHashcode {

  override def isFinal(sid: RichSid, rhs: TopLevelConstraint): Boolean = false

  override def decompsOrEmptySet: Set[ContextDecomposition] = Set.empty

  override def renameOrFail(sid: RichSid, to: Seq[Var]): Option[EntailmentProfile] = {
    // TODO: Some code duplication with the rename operation of decomposable profiles
    val instantiation = orderedParams zip to
    val update = InstantiationUpdate(instantiation, sharedConstraints.classes)
    for {
      updated <- update(sharedConstraints)
    } yield ProfileOfNondecomposableModels(updated, to.distinct.filterNot(_.isNull))
  }

  override def forget(varsToForget: Set[Var]): EntailmentProfile = {
    val newConstraints = DropperUpdate(varsToForget).unsafeUpdate(sharedConstraints)
    ProfileOfNondecomposableModels(newConstraints, orderedParams.filterNot(varsToForget))
  }

  override def toString: String = {
    s"ProfileOfNondecomposableModels($sharedConstraints)"
  }

  override def isConsistentWithFocus(sid: RichSid): Boolean = true

  override def applyToDecomps(f: Set[ContextDecomposition] => Set[ContextDecomposition]): EntailmentProfile = this

}

object ProfileOfNondecomposableModels {

  def apply(orderedParams: Seq[Var]): ProfileOfNondecomposableModels = ProfileOfNondecomposableModels(VarConstraints.fromAtoms(orderedParams, Seq.empty), orderedParams)

}

case class ProfileOfDecomps(decomps: Set[ContextDecomposition], override val sharedConstraints: VarConstraints, override val orderedParams: Seq[Var]) extends EntailmentProfile with HarrshLogging with CachedHashcode {

  assert(decomps.nonEmpty)

  assert(decomps forall (_.isInPlaceholderNormalForm),
    s"The decomposition ${decomps.find(!_.isInPlaceholderNormalForm).get} is not in placeholder normalform"
  )

  override def decompsOrEmptySet: Set[ContextDecomposition] = decomps

  private val nonPlaceholderVarsInContexts = decomps.flatMap(_.nonNullNonPlaceholderVars)

  lazy val guaranteedAllocated: Set[Var] = {
    // Note: There can be differences between allocation depending on "speculative" equalities imposed by picking specific ways to construct parse trees.
    // To quickly find all benchmarks that exhibit this behavior, run batch mode after uncommenting this assertion and removing the "lazy" modifier
    //assert(decomps.map(_.allocedVars).size <= 1, s"Differences in allocated vars of decompositions (${decomps.map(_.allocedVars)}) in profile $this")
    decomps.map(_.allocedVars).reduce(_ intersect _)
  }

  if (!(nonPlaceholderVarsInContexts subsetOf orderedParams.toSet)) {
    throw new IllegalArgumentException(s"Constructing state for $orderedParams, but profile contains FVs $nonPlaceholderVarsInContexts:\n${decomps.mkString("\n")}")
  }

  override def isFinal(sid: RichSid, rhs: TopLevelConstraint): Boolean = {
    // A profiles represents all the possible ways to parse a symbolic heap as an unfolding forest
    // As long as one of those ways is a valid unfolding forest w.r.t. rhs, we accept.
    val res = decomps.exists(_.isFinal(sid, rhs))
    logger.trace(s"Checked wheter $this is final => $res")
    res
  }

  private def renameDecomps(sid: RichSid, instantiationUpdateFactory: Set[Set[Var]] => InstantiationUpdate): Set[ContextDecomposition] = {
    val renamed = for {
      decomp <- decomps
      update = instantiationUpdateFactory(decomp.constraints.classes)
      instantiated <- decomp.updateSubst(update)
    } yield instantiated
    // TODO Do we want to improve the consistency check, which currently only looks at root parameters? (But that would require knowing which other variables are guaranteed to be allocated in the SID, i.e., additional preprocessing!)
    renamed filter (_.isConsistentWithFocus(sid)) map (_.toPlaceholderNormalForm)
  }

  private def instantiationUpdateFactory(to: Seq[Var]): Set[Set[Var]] => InstantiationUpdate = {
    assert(to.size == orderedParams.size)
    val instantiation = orderedParams zip to
    classes =>
      InstantiationUpdate(instantiation, classes)
  }

  override def renameOrFail(sid: RichSid, to: Seq[Var]): Option[EntailmentProfile] = {
    logger.debug(s"Will rename $orderedParams to $to in $this")
    val updateFactory = instantiationUpdateFactory(to)
    val res = for {
      // Rename shared constraints first, since this is generally where the inconsistency in renaming will come in (not only in the speculation),
      // so doing this first allows us to short-circuit the renaming process
      renamedSharedConstraints <- updateFactory(sharedConstraints.classes)(sharedConstraints)
      renamedDecomps = renameDecomps(sid, updateFactory)
      renamedParams = to.distinct.filterNot(_.isNull)
      if renamedDecomps.nonEmpty
    } yield ProfileOfDecomps(renamedDecomps, renamedSharedConstraints, renamedParams)

    if (res.isEmpty) {
      logger.debug(s"Renaming the FVs in $this to $to yielded an inconsistent result => Discarding result profile")
    }
    res
  }

  override def forget(varsToForget: Set[Var]): EntailmentProfile = {

    def filterDecomps(decomps: Set[ContextDecomposition]) = {
      for {
        decomp <- decomps
        restrictedToFreeVars <- decomp.forget(varsToForget)
        _ = logger.debug(s"Has enough names after restriction to free variables:\n$restrictedToFreeVars; will keep if viable.")
        _ = assert(restrictedToFreeVars.boundVars.isEmpty, s"Bound vars remain after restriction to free vars: $restrictedToFreeVars")
      } yield restrictedToFreeVars
    }

    val newParams = orderedParams filterNot varsToForget
    val newSharedConstraints = DropperUpdate(varsToForget).unsafeUpdate(sharedConstraints)
    val newDecomps = filterDecomps(decomps)
    if (newDecomps.isEmpty) {
      ProfileOfNondecomposableModels(newSharedConstraints, newParams)
    } else {
      ProfileOfDecomps(newDecomps, newSharedConstraints, newParams)
    }
  }

  def isConsistentWithFocus(sid: RichSid): Boolean = {
    decomps forall (_.isConsistentWithFocus(sid))
  }

  def applyToDecomps(f: Set[ContextDecomposition] => Set[ContextDecomposition]): EntailmentProfile = {
    val afterF = f(decomps)
    if (afterF.nonEmpty) {
      this.copy(decomps = afterF)
    } else {
      ProfileOfNondecomposableModels(sharedConstraints, orderedParams)
    }
  }

  override def toString: String = {
    "ProfileOfDecomps(\n" + decomps.mkString(",\n") + ",\n  params = " + orderedParams.mkString(", ") + "\n)"
  }

}