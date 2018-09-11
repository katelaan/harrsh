package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.PredCall
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, Var}

case class TreeInterface private(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker) extends HarrshLogging {

  assert(NodeLabel.noRedundantPlaceholders(labels), s"There are redundant placeholders in $this")

  lazy val labels: Seq[NodeLabel] = Seq[NodeLabel](root) ++ leaves

  lazy val placeholders: Set[PlaceholderVar] = substs.flatMap(_.placeholders).toSet

  lazy val boundVars: Set[BoundVar] = substs.flatMap(_.boundVars).toSet

  private lazy val substs = labels map (_.subst)

  override def toString: String = {
    val leavesString = if (leaves.isEmpty) "empty" else leaves.mkString(",")
    val usageStr = usageInfo.map{
      case (vs, usage) => vs.mkString(",") + "->" + usage
    }.mkString("; ")
    val ensuredStr = pureConstraints.ensured.mkString(",")
    val missingStr = pureConstraints.missing.mkString(",")
    s"TI(root = $root; leaves = $leavesString; usage = { $usageStr }; ensured = { $ensuredStr }; missing = { $missingStr })"
  }

  def isConcrete: Boolean = leaves.isEmpty

  def hasConsistentPureConstraints: Boolean = pureConstraints.isConsistent

  def hasNamesForRootParams: Boolean = labels.forall{
    label =>
      label.pred.rootParamIndex match {
        case Some(rootParamIndex) =>
          val labelingVars: Set[Var] = label.subst.toSeq(rootParamIndex)
          labelingVars.exists(v => v.isFreeNonNull && !PlaceholderVar.isPlaceholder(v))
        case None =>
          // No root parameter to have a name for
          true
      }
  }

  def isFinalFor(call: PredCall): Boolean = {
    val rootPred = root.pred
    Stream(
      isConcrete, // It represents a concrete tree...
      rootPred.head == call.name, // ...rooted in the correct predicate...
      pureConstraints.missing.isEmpty, // ...without missing pure constraints...
      (call.args, root.subst.toSeq).zipped.forall{
        // ...and with the correct vector of variables at the root (corresponding to the goal predicate call)
        case (arg, substVal) => substVal.contains(arg)
      }
    ).forall(b => b)
  }

  def asExtensionType: ExtensionType = ExtensionType(Set(this))

  def nonPlaceholderFreeVars: Set[FreeVar] = {
    substs.flatMap(_.freeNonNullVars).filterNot(PlaceholderVar.isPlaceholder).toSet
  }

  def updateSubst(f: SubstitutionUpdate, convertToNormalform: Boolean): TreeInterface = {
    TreeInterface(root.update(f),
      leaves map (_.update(f)),
      VarUsageByLabel.update(usageInfo, f),
      pureConstraints.update(f),
      convertToNormalform)
  }

  def dropVarsFromPureConstraints(varsToDrop: Set[Var]): TreeInterface = {
    TreeInterface(root, leaves, usageInfo, pureConstraints.dropVars(varsToDrop), convertToNormalform = false)
  }

  def usageInfoOfNode(n: NodeLabel): VarUsageInfo = {
    val res = n.subst.toSeq.map(usageInfo.getOrElse(_, VarUsage.Unused))
    logger.trace(s"Usage info for $n w.r.t. $this: $res")
    res
  }

  def allRootParamsUsed: Boolean = {
    val rootsUsed = for {
      node <- labels.toStream
      rootParam <- node.pred.rootParam
      ix = node.freeVarSeq.indexOf(rootParam)
      usage = usageInfoOfNode(node)
    } yield usage(ix).isUsed
    rootsUsed.forall(b => b)
  }

  def hasNamesForUsedParams: Boolean = {
    // Everything that's used has a name
    val enoughNamesByNodeAndParam = for {
      node <- labels.toStream
      usageByVar = (node.subst.toSeq, usageInfoOfNode(node)).zipped
      (substVars, usg) <- usageByVar
      res = !usg.isUsed || substVars.exists(!PlaceholderVar.isPlaceholder(_))
      _ = {
        if (!res) logger.debug(s"Not enough names for $node: $substVars is not marked as used")
      }
    } yield res
    enoughNamesByNodeAndParam.forall(b => b)
  }
}

object TreeInterface {

  implicit val canComposeTreeInterfaces: CanCompose[TreeInterface] = CanComposeTreeInterface.canComposeTreeInterfaces

  def apply(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker, convertToNormalform: Boolean): TreeInterface = {
    if (convertToNormalform) {
      normalFormConversion(root, leaves, usageInfo, pureConstraints)
    } else {
      new TreeInterface(root, leaves, usageInfo, pureConstraints)
    }
  }

  def normalFormConversion(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker): TreeInterface = {
    val dropper = SubstitutionUpdate.redundantPlaceholderDropper(Set(root) ++ leaves)
    val rootAfterDropping = root.update(dropper)
    val leavesAfterDropping = leaves map (_.update(dropper))
    val usageInfoAfterDropping = VarUsageByLabel.update(usageInfo, dropper)
    val diseqsAfterDropping = pureConstraints.update(dropper)

    val establishNormalForm = NodeLabel.labelsToPlaceholderNormalForm(Seq(rootAfterDropping) ++ leavesAfterDropping)

    new TreeInterface(
      rootAfterDropping.update(establishNormalForm),
      leavesAfterDropping map (_.update(establishNormalForm)),
      VarUsageByLabel.update(usageInfoAfterDropping, establishNormalForm),
      diseqsAfterDropping.update(establishNormalForm))
  }

  def isInNormalForm(tif: TreeInterface): Boolean = {
    NodeLabel.noRedundantPlaceholders(tif.labels) && PlaceholderVar.noGapsInPlaceholders(tif.placeholders)
  }

  def haveNoConflicts(tif1: TreeInterface, tif2: TreeInterface): Boolean = {
    (tif1.placeholders intersect tif2.placeholders).isEmpty
  }

}