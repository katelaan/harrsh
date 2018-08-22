package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.PredCall

case class TreeInterface private(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel) {

  assert(TreeInterface.noRedundantPlaceholders(this),
    s"There are redundant placeholders in $this"
  )

  lazy val labels = Seq[NodeLabel](root) ++ leaves

  override def toString: String = {
    val usageStr = usageInfo.mkString(",")
    s"TI(root = $root; leaves = ${leaves.mkString(",")}; usage = $usageStr)"
  }

  def isConcrete: Boolean = leaves.isEmpty

  def hasNamesForRootParams: Boolean = labels.forall{
    label =>
      // TODO: Get the index directly through the pred. (Change this in other places, too)
      val rootParam = label.pred.rootParam.get
      val rootParamIndex = label.pred.params.indexOf(rootParam)
      val labelingVars: Set[Var] = label.subst.toSeq(rootParamIndex)
      labelingVars.exists(v => v.isFreeNonNull && !PlaceholderVar.isPlaceholder(v))
  }

  def asExtensionType: ExtensionType = ExtensionType(Set(this))

  def nonPlaceholderFreeVars: Set[FreeVar] = {
    substs.flatMap(_.freeNonNullVars).filterNot(PlaceholderVar.isPlaceholder).toSet
  }

  def placeholderVarsInSubst: Set[PlaceholderVar] = {
    substs.flatMap(_.placeholders).toSet
  }

  def updateSubst(f: SubstitutionUpdate): TreeInterface = {
    new TreeInterface(root.update(f), leaves map (_.update(f)), VarUsageByLabel.update(usageInfo,f))
  }

  private lazy val substs = labels map (_.subst)
}

object TreeInterface {

  implicit val canComposeTreeInterfaces: CanCompose[TreeInterface] = CanComposeTreeInterface.canComposeTreeInterfaces

  def apply(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel], usageInfo: VarUsageByLabel): TreeInterface = {
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

  def haveNoConflicts(tif1: TreeInterface, tif2: TreeInterface) : Boolean = {
    (tif1.placeholderVarsInSubst intersect tif2.placeholderVarsInSubst).isEmpty
  }

  // TODO: Check this in UTs as well?
  def noRedundantPlaceholders(tif: TreeInterface): Boolean = {
    tif.labels.forall{
      nodeLabel => nodeLabel.subst.toSeq.forall(containsNoRedundantPlaceholder)
    }
  }

  private def containsNoRedundantPlaceholder(vs: Set[Var]): Boolean = {
    // There's at most one placeholder in the set and if there's one in the set it's the only element
    val numPhs = vs.count(PlaceholderVar.isPlaceholder)
    numPhs <= 1 && (numPhs == 0 || vs.size == 1)
  }

  def noGapsInPlaceholders(tif: TreeInterface): Boolean = {
    val phs = tif.placeholderVarsInSubst
    phs.isEmpty || phs.map(_.index).max == phs.size
  }

}

/**
  * A single abstracted forest, retaining only the tree interfaces rather than the full trees.
  * @param parts Abstracted trees
  */
case class ExtensionType(parts: Set[TreeInterface]) extends HarrshLogging {

  override def toString: String = parts.mkString("ET(", ",\n   ", ")")

  def hasNamesForRootParams: Boolean = parts.forall(_.hasNamesForRootParams)

  def isFinal(call: PredCall): Boolean = {
    val res = if (parts.size != 1) {
      // It's not a single (abstracted) tree, so it can't represent a single concrete unfolding tree
      false
    } else {
      val tif = parts.head
      val rootPred = tif.root.pred
      Stream(
        tif.isConcrete, // It represents a concrete tree...
        rootPred.head == call.name, // ...rooted in the correct predicate...
        (call.args, tif.root.subst.toSeq).zipped.forall{
          // ...and with the correct vector of variables at the root (corresponding to the goal predicate call)
          case (arg, substVal) => substVal.contains(arg)
        }
      ).forall(b => b)
    }
    logger.debug(s"Checking whether $this is final w.r.t. $call => $res")
    res
  }

  def nonPlaceholderFreeVars: Set[FreeVar] = parts.flatMap(_.nonPlaceholderFreeVars)

  def placeholdersInSubst: Set[PlaceholderVar] = parts.flatMap(_.placeholderVarsInSubst)

  def dropVars(varsToDrop: Seq[Var]): ExtensionType = {
    // TODO: More efficient variable dropping for extension types
    // FIXME: Don't introduce redundant placeholder vars, i.e., if there is a variable alias that we don't drop, we don't need a new placeholder name to avoid empty sets in the substitution
    // 1) Rename the vars to placeholder vars
    val placeholders = (1 to varsToDrop.size) map (PlaceholderVar(_))
    val pairs: Seq[(Var,Var)] = varsToDrop.zip(placeholders.map(_.toFreeVar))
    val replaceByPlacheolders: SubstitutionUpdate = SubstitutionUpdate.fromPairs(pairs)

    // 0) To avoid clashes, shift the existing vars back
    val shiftPlaceholders = PlaceholderVar.placeholderClashAvoidanceUpdate(placeholders.toSet)

    updateSubst(shiftPlaceholders).updateSubst(replaceByPlacheolders)
  }

  def updateSubst(f: SubstitutionUpdate): ExtensionType = ExtensionType(parts map (_.updateSubst(f)))

  def compose(other: ExtensionType): ExtensionType = {
    val composed = CanCompose.composeAll(parts.toSeq ++ other.parts)
    ExtensionType(composed)
  }

}

object ExtensionType {

  def apply(parts: Seq[TreeInterface]): ExtensionType = ExtensionType(parts.toSet)

}
