package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.RichSid
import at.forsyte.harrsh.seplog.{BoundVar, NullConst, Var}

case class OldEntailmentContext private(root: ContextPredCall, calls: Set[ContextPredCall], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker) extends HarrshLogging {

  assert(OldEntailmentContext.noRedundantPlaceholders(labels), s"There are redundant placeholders in $this")
  assert(OldEntailmentContext.nodeLabelsAndUsageInfoContainSameVars(this), s"Inconsistent entailment context $this: There is a difference between the variables in the usage info and in the substitutions")

  lazy val labels: Seq[ContextPredCall] = Seq(root) ++ calls

  lazy val placeholders: Set[PlaceholderVar] = substs.flatMap(_.placeholders).toSet

  lazy val boundVars: Set[BoundVar] = substs.flatMap(_.boundVars).toSet

  def impliesNullAllocation(sid: RichSid): Boolean = {
    allocedParamSubsts.exists(_.contains(NullConst)) || calls.exists(_.hasNullInRootPosition(sid))
  }

  def hasNullInRootPosition(sid: RichSid): Boolean = {
    calls.exists(_.hasNullInRootPosition(sid))
  }

  //def allocatesNull(sid: RichSid): Boolean = labels.exists(_.rootParamSubst(sid).getOrElse(Set.empty).contains(NullConst))

  private lazy val substs = labels map (_.subst)

  override def toString: String = {
    val callsString = if (calls.isEmpty) "empty" else calls.mkString(",")
    val usageStr = usageInfo.map{
      case (vs, usage) => vs.mkString(",") + ": " + usage.shortString
    }.mkString("; ")
    val ensuredStr = if (pureConstraints.ensured.nonEmpty) {
      pureConstraints.ensured.mkString("; ensured = {", ",", "}")
    } else ""
    val missingStr = if (pureConstraints.missing.nonEmpty) {
      pureConstraints.missing.mkString("; missing = {", ",", "}")
    } else ""
    s"Ctx(root = $root; calls = $callsString; usage = {$usageStr}$ensuredStr$missingStr)"
  }

  def isConcrete: Boolean = calls.isEmpty

  def hasConsistentPureConstraints: Boolean = pureConstraints.isConsistent

  def hasNonNullNamesForRootParams(sid: RichSid): Boolean = rootParamSubsts(sid).forall {
    labelingVars => labelingVars.exists(PlaceholderVar.isNonPlaceholderNonNullFreeVar)
  }

  def rootParamSubsts(sid: RichSid): Seq[Set[Var]] = labels flatMap (_.rootParamSubst(sid))

  def asExtensionType: OldContextDecomposition = OldContextDecomposition(Set(this))

  def nonNullNonPlaceholderVars: Set[Var] = {
    substs.flatMap(_.nonNullVars).filterNot(PlaceholderVar.isPlaceholder).toSet
  }

  def updateSubst(f: SubstitutionUpdate, convertToNormalform: Boolean): OldEntailmentContext = {
    OldEntailmentContext(root.update(f),
      calls map (_.update(f)),
      VarUsageByLabel.update(usageInfo, f),
      pureConstraints.update(f),
      convertToNormalform)
  }

  def dropVarsFromPureConstraints(varsToDrop: Set[Var]): OldEntailmentContext = {
    OldEntailmentContext(root, calls, usageInfo, pureConstraints.dropVars(varsToDrop), convertToNormalform = false)
  }

  def usageInfoOfNode(n: ContextPredCall): VarUsageInfo = {
    val res = n.subst.toSeq.map(usageInfo.getOrElse(_, VarUnused))
    logger.debug(s"Usage info for $n w.r.t. $this: $res")
    res
  }

  def hasNamesForUsedParams: Boolean = usedParamSubst.forall {
    labelingVars => labelingVars.exists(PlaceholderVar.isNonPlaceholderFreeVar)
  }

  def allocedParamSubsts: Stream[Set[Var]] = for {
    (vars, usage) <- usageInfo.toStream
    if usage == VarAllocated
  } yield vars

  def usedParamSubst: Stream[Set[Var]] = for {
    (vars, usage) <- usageInfo.toStream
    if usage != VarUnused
  } yield vars
}

object OldEntailmentContext extends HarrshLogging {

  def apply(root: ContextPredCall, leaves: Set[ContextPredCall], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker, convertToNormalform: Boolean): OldEntailmentContext = {
    if (convertToNormalform) {
      normalFormConversion(root, leaves, usageInfo, pureConstraints)
    } else {
      new OldEntailmentContext(root, leaves, usageInfo, pureConstraints)
    }
  }

  def normalFormConversion(root: ContextPredCall, leaves: Set[ContextPredCall], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker): OldEntailmentContext = {
    val dropper = SubstitutionUpdate.redundantPlaceholderDropper(Set(root) ++ leaves)
    val rootAfterDropping = root.update(dropper)
    val leavesAfterDropping = leaves map (_.update(dropper))
    val usageInfoAfterDropping = VarUsageByLabel.update(usageInfo, dropper)
    val diseqsAfterDropping = pureConstraints.update(dropper)

    val establishNormalForm = ContextPredCall.placeholderNormalFormUpdater(Seq(rootAfterDropping) ++ leavesAfterDropping)

    new OldEntailmentContext(
      rootAfterDropping.update(establishNormalForm),
      leavesAfterDropping map (_.update(establishNormalForm)),
      VarUsageByLabel.update(usageInfoAfterDropping, establishNormalForm),
      diseqsAfterDropping.update(establishNormalForm))
  }

  def isInNormalForm(ctx: OldEntailmentContext): Boolean = {
    noRedundantPlaceholders(ctx.labels) && PlaceholderVar.noGapsInPlaceholders(ctx.placeholders)
  }

  def haveNoConflicts(ctx1: OldEntailmentContext, ctx2: OldEntailmentContext): Boolean = {
    (ctx1.placeholders intersect ctx2.placeholders).isEmpty
  }

  def noRedundantPlaceholders(labels: Iterable[ContextPredCall]): Boolean = {
    labels.forall{
      nodeLabel => nodeLabel.subst.toSeq.forall(PlaceholderVar.containsNoRedundantPlaceholder)
    }
  }

  def nodeLabelsAndUsageInfoContainSameVars(ctx: OldEntailmentContext): Boolean = {
    val varsOccurringInNodeLabels: Set[Set[Var]] = ctx.substs.toSet[Substitution].flatMap(subst => subst.toSeq)
    val varsInUsageInfo = ctx.usageInfo.keySet
    logger.debug(s"Vars in node labels: $varsOccurringInNodeLabels / Vars in usage info: $varsInUsageInfo")
    // FIXME: [ENTAILMENT CLEANUP] Does it make sense to drop null here?
    // FIXME: [ENTAILMENT CLEANUP] Do we really have to enforce that the variables are the same? Isn't it enough that we have usage info for everything that occurs in the nodes? (Supposing that forgetting free vars works correctly, that should be sound? It might even be the only sound thing to do, because we might otherwise lose info about double allocation?)
    // varsOccurringInNodeLabels == varsInUsageInfo
    //varsOccurringInNodeLabels.filterNot(_.contains(NullConst)) == varsInUsageInfo.filterNot(_.contains(NullConst))
    varsOccurringInNodeLabels.filterNot(_.contains(NullConst)) subsetOf varsInUsageInfo.filterNot(_.contains(NullConst))
  }

}