package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, Var}

case class TreeInterface private(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel) {

  assert(NodeLabel.noRedundantPlaceholders(labels), s"There are redundant placeholders in $this")

  lazy val labels = Seq[NodeLabel](root) ++ leaves

  lazy val placeholders: Set[PlaceholderVar] = substs.flatMap(_.placeholders).toSet

  lazy val boundVars: Set[BoundVar] = substs.flatMap(_.boundVars).toSet

  private lazy val substs = labels map (_.subst)

  override def toString: String = {
    val leavesString = if (leaves.isEmpty) "empty" else leaves.mkString(",")
    val usageStr = usageInfo.map{
      case (vs, usage) => vs.mkString(",") + "->" + usage
    }.mkString("; ")
    s"TI(root = $root; leaves = $leavesString; usage = { $usageStr })"
  }

  def isConcrete: Boolean = leaves.isEmpty

  def hasNamesForRootParams: Boolean = labels.forall{
    label =>
      val rootParamIndex = label.pred.rootParamIndex.get
      val labelingVars: Set[Var] = label.subst.toSeq(rootParamIndex)
      labelingVars.exists(v => v.isFreeNonNull && !PlaceholderVar.isPlaceholder(v))
  }

  def asExtensionType: ExtensionType = ExtensionType(Set(this))

  def nonPlaceholderFreeVars: Set[FreeVar] = {
    substs.flatMap(_.freeNonNullVars).filterNot(PlaceholderVar.isPlaceholder).toSet
  }

  def updateSubst(f: SubstitutionUpdate, convertToNormalform: Boolean): TreeInterface = {
    TreeInterface(root.update(f), leaves map (_.update(f)), VarUsageByLabel.update(usageInfo,f), convertToNormalform = convertToNormalform)
  }
}

object TreeInterface {

  implicit val canComposeTreeInterfaces: CanCompose[TreeInterface] = CanComposeTreeInterface.canComposeTreeInterfaces

  def apply(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel, convertToNormalform: Boolean): TreeInterface = {
    if (convertToNormalform) {
      normalFormConversion(root, leaves, usageInfo)
    } else {
      new TreeInterface(root, leaves, usageInfo)
    }
  }

  def normalFormConversion(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel): TreeInterface = {
    val dropper = SubstitutionUpdate.redundantPlaceholderDropper(Set(root) ++ leaves)
    val rootAfterDropping = root.update(dropper)
    val leavesAfterDropping = leaves map (_.update(dropper))
    val usageInfoAfterDropping = VarUsageByLabel.update(usageInfo, dropper)

    val establishNormalForm = NodeLabel.labelsToPlaceholderNormalForm(Seq(rootAfterDropping) ++ leavesAfterDropping)

    new TreeInterface(
      rootAfterDropping.update(establishNormalForm),
      leavesAfterDropping map (_.update(establishNormalForm)),
      VarUsageByLabel.update(usageInfoAfterDropping, establishNormalForm))
  }

  def isInNormalForm(tif: TreeInterface): Boolean = {
    NodeLabel.noRedundantPlaceholders(tif.labels) && PlaceholderVar.noGapsInPlaceholders(tif.placeholders)
  }

  def haveNoConflicts(tif1: TreeInterface, tif2: TreeInterface): Boolean = {
    (tif1.placeholders intersect tif2.placeholders).isEmpty
  }

}