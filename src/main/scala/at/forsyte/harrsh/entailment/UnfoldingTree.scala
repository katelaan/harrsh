package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.ToLatex

case class UnfoldingTree private(nodeLabels: Map[NodeId,NodeLabel], root: NodeId, children: Map[NodeId, Seq[NodeId]]) extends HarrshLogging {

  import at.forsyte.harrsh.entailment.UnfoldingTree._

  lazy val nodes: Set[NodeId] = nodeLabels.keySet

  lazy val abstractLeaves: Set[NodeId] = nodes.filter(n => nodeLabels(n).isAbstractLeaf)

  def isAbstractLeaf(nodeId: NodeId): Boolean = nodeLabels(nodeId).isAbstractLeaf

  assert(nodes.contains(root))
  assert(children.keys forall nodes.contains)
  assert(children.values forall (_ forall nodes.contains))
  assert(abstractLeaves forall (children(_).isEmpty))

  assert(nodes forall (n => children(n).nonEmpty
    || isAbstractLeaf(n)
    || (!isAbstractLeaf(n) && nodeLabels(n).asInstanceOf[RuleNodeLabel].rule.isBaseRule)),
    s"Inconsistent unfolding tree $this")

  lazy val parents: Map[NodeId, NodeId] = {
    for {
      (parent, succs) <- children
      child <- succs
    } yield (child, parent)
  }

  override def toString: String = {
    val sb = new StringBuilder("UnfoldingTree(\n")
    for {
      id <- nodes.toSeq.sorted
    } {
      sb.appendAll(s"  $id -> ${nodeLabels(id)} : ${children(id).mkString(", ")}\n")
    }
    sb.append(')')
    sb.toString
  }

  def isConcrete: Boolean = abstractLeaves.isEmpty

  def interface: TreeInterface = {
    val interfaceNodes: Seq[NodeLabel] = Seq(nodeLabels(root)) ++ (abstractLeaves map (nodeLabels(_)))
    val allUsage: Seq[(Set[Var], VarUsage)] = for {
      node <- interfaceNodes
      (v,ix) <- node.freeVarSeq.zipWithIndex
    } yield (node.subst.toSeq(ix), findUsage(node, v))
    val allUsageByLabel = allUsage.groupBy(_._1)
    val usageInfoMap: Map[Set[Var], VarUsage] = allUsageByLabel.map{
      case (lbl, usage) => (lbl, usage.map(_._2).max)
    }
    logger.debug(s"Usage map for $this: $usageInfoMap")
    TreeInterface(nodeLabels(root), abstractLeaves map (nodeLabels(_).asInstanceOf[AbstractLeafNodeLabel]), usageInfoMap, convertToNormalform = true)
  }

  def unfold(leaf: NodeId, sid: SID, rule: RuleBody): UnfoldingTree = {
    // TODO: Split method into smaller pieces
    assert(abstractLeaves.contains(leaf))

    val pred = nodeLabels(leaf).pred
    assert(pred.rules.contains(rule))

    val leafSubst = nodeLabels(leaf).subst
    val rootLabel = RuleNodeLabel(pred, rule, leafSubst)
    val boundVars = rule.body.boundVars
    val allUnusedPlaceholders = for {
      i <- Stream.from(0)
      pv = PlaceholderVar(i)
      if !leafSubst.placeholders.contains(pv)
    } yield pv.toFreeVar
    val boundVarsToPlaceholders = boundVars.zip(allUnusedPlaceholders).toMap

    val mkSubst = {
      args: Seq[Var] =>
        val targets = args.map {
          case fv: FreeVar => Set[Var](fv)
          case NullConst => throw new NotImplementedError
          case bv:BoundVar => Set[Var](boundVarsToPlaceholders(bv))
        }
        Substitution(targets)
    }

    val childLabels = rule.body.predCalls map (call => AbstractLeafNodeLabel(sid(call.name), mkSubst(call.args)))
    val ids = NodeId.freshIds(Set.empty, childLabels.size + 1)
    val ruleTreeNodeLabels = (ids, rootLabel +: childLabels).zipped.toMap
    val ruleTreeChildTuples = (ids.head -> ids.tail) +: ids.tail.zip(Stream.continually(Seq.empty))

    val ruleTree = UnfoldingTree(
      nodeLabels = ruleTreeNodeLabels,
      root = ids.head,
      children = ruleTreeChildTuples.toMap,
      convertToNormalform = false
    )
    val shiftedRuleTree = CanCompose[UnfoldingTree].makeDisjoint(ruleTree, this)._1

    // Shifting may have renamed placeholder vars at the root (if there are any), which we revert through unification
    val rootSubst = shiftedRuleTree.nodeLabels(shiftedRuleTree.root).subst
    val unification: Unification = (leafSubst.toSeq, rootSubst.toSeq).zipped.map(_ union _)

    instantiate(leaf, shiftedRuleTree, unification)
  }

  def project(retainCalls: Boolean = false): SymbolicHeap = {

    def projectNode(node: NodeId): SymbolicHeap = {
      val label = nodeLabels(node)
      val withoutSubst = label match {
        case RuleNodeLabel(_, rule, _) =>
          val childProjections = children(node) map projectNode
          rule.body.replaceCalls(childProjections)
        case AbstractLeafNodeLabel(pred, _) =>
          if (retainCalls) pred.defaultCall else SymbolicHeap.empty
      }
      withoutSubst.copy(pure = withoutSubst.pure ++ label.subst.toAtoms(label.pred.params))
    }

    projectNode(root)
  }

  def instantiate(abstractLeaf: NodeId, replacingTree: UnfoldingTree, unification: Unification): UnfoldingTree = {
    assert(haveNoConflicts(this, replacingTree))
    logger.debug(s"Replacing $abstractLeaf in $this with $replacingTree")
    val propagateUnification = SubstitutionUpdate.fromUnification(unification)
    val thisExtended = this.updateSubst(propagateUnification, convertToNormalform = false)
    val otherExtended = replacingTree.updateSubst(propagateUnification, convertToNormalform = false)
    // TODO: This instantiation 'leaks' the ID of abstractLeaf: It will not be used in the tree that we get after instantiation. I'm afraid this may complicate debugging.
    val combinedNodeLabels = (thisExtended.nodeLabels ++ otherExtended.nodeLabels) - abstractLeaf

    // Connect the parent of the replaced leaf with the root of the replacing tree
    val maybeParent = thisExtended.parents.get(abstractLeaf)
    val updatedChildren = maybeParent match {
      case Some(parent) =>
        val newParentsChildren = thisExtended.children(parent).map{
          child => if (child == abstractLeaf) otherExtended.root else child
        }
        logger.debug(s"Updating children of $parent from ${thisExtended.children(parent)} to $newParentsChildren")
        thisExtended.children.updated(parent, newParentsChildren)
      case None =>
        thisExtended.children
    }

    // TODO: Another position where the 'leak' manifests
    val combinedChildren = (updatedChildren ++ otherExtended.children) - abstractLeaf

    val newRoot = if (maybeParent.nonEmpty) {
      // The replaced leaf wasn't the root => The root remains unchanged
      thisExtended.root
    } else {
      // The replace leaf was the root => The new root is the root of the replacing tree
      otherExtended.root
    }

    UnfoldingTree(combinedNodeLabels, newRoot, combinedChildren, convertToNormalform = true)
  }

  def placeholders: Set[PlaceholderVar] = nodeLabels.values.flatMap(_.placeholders).toSet

  def updateSubst(f: SubstitutionUpdate, convertToNormalform: Boolean): UnfoldingTree = {
    val updatedLabels = nodeLabels.map {
      case (id,label) => (id, label.update(f))
    }
    UnfoldingTree(updatedLabels, root, children, convertToNormalform = convertToNormalform)
  }

  def findUsage(n: NodeLabel, v: FreeVar): VarUsage = {
    // TODO: Usage tracking/discovery is extremely inefficient but occurs very frequently! (E.g. looking up the node id for the node in O(n))

    // A variable is used if either...
    // 1.) There is a free variable somewhere in the UT that is used and that's equivalent
    val s: Set[Var] = n.subst.toSeq(n.freeVarSeq.indexOf(v))
    val usages = for {
      nodeLabel <- nodeLabels.values
      substSeq = nodeLabel.subst.toSeq
      (vars, ix) <- substSeq.zipWithIndex
      if vars == s
    } yield nodeLabel.varUsage(nodeLabel.freeVarSeq(ix))

    // or 2.) v is equivalent to a bound variable in its father and that variable is used
    val maybeParentUsage: Option[VarUsage] = for {
      (nodeId, _) <- nodeLabels.find(_._2 == n)
      parent <- parents.get(nodeId)
      parentLabel = nodeLabels(parent).asInstanceOf[RuleNodeLabel]
      sh = parentLabel.rule.body
      callsWithChildren: Seq[(PredCall, NodeId)] = sh.predCalls.zip(children(parent))
      (callForN, _) <- callsWithChildren.find(_._2 == nodeId)
      matchingVar = callForN.args(n.freeVarSeq.indexOf(v))
      ptr = sh.pointers.head
    } yield {
      // FIXME: This doesn't take into account that 'matchingVar' could only be equal to an allocated/referenced var rather than used itself!
      if (ptr.from == matchingVar) VarUsage.Allocated
      else if (ptr.to.contains(matchingVar)) VarUsage.Referenced
      else VarUsage.Unused
    }

    val allUsages = usages ++ maybeParentUsage

    if (allUsages.isEmpty) VarUsage.Unused else allUsages.max
  }

}

object UnfoldingTree extends HarrshLogging {

  implicit val treeToLatex: ToLatex[UnfoldingTree] = ForestsToLatex.treeToLatex
  implicit val canComposeUT: CanCompose[UnfoldingTree] = CanComposeUnfoldingTree.canComposeUT

  def apply(nodeLabels: Map[NodeId,NodeLabel], root: NodeId, children: Map[NodeId, Seq[NodeId]], convertToNormalform: Boolean): UnfoldingTree = {
    val processedNodeLabels = if (convertToNormalform) normalFormConversion(nodeLabels, root, children) else nodeLabels
    new UnfoldingTree(processedNodeLabels, root, children)
  }

  private def getSubstOrDefault(subst: Option[Substitution], pred: Predicate): Substitution = {
    subst.getOrElse(Substitution.identity(pred.params))
  }

  def singleton(sid: SID, pred: Predicate, subst: Option[Substitution] = None): UnfoldingTree = {
    val label = AbstractLeafNodeLabel(pred, getSubstOrDefault(subst, pred))
    val id = NodeId.zero
    UnfoldingTree(Map(id -> label), id, Map(id -> Seq.empty), convertToNormalform = false)
  }

  def fromPredicate(sid: SID, pred: String, labeling: Substitution): UnfoldingTree = {
    val node = AbstractLeafNodeLabel(sid(pred), labeling)
    UnfoldingTree(Map(NodeId.zero -> node), NodeId.zero, Map.empty[NodeId, Seq[NodeId]], convertToNormalform = false)
  }

  def haveNoConflicts(ut1: UnfoldingTree, ut2: UnfoldingTree) : Boolean = {
    (ut1.nodes intersect ut2.nodes).isEmpty && (ut1.placeholders intersect ut2.placeholders).isEmpty
  }

  def isInNormalForm(ut: UnfoldingTree): Boolean = {
    // TODO: Check that placeholders are introduced in BFS-order
    NodeLabel.noRedundantPlaceholders(ut.nodeLabels.values) && PlaceholderVar.noGapsInPlaceholders(ut.placeholders)
  }

  /**
    * Update node labels to ensure that there are no redundant placeholders etc.
    */
  object normalFormConversion {

    def apply(nodeLabels: Map[NodeId,NodeLabel], root: NodeId, children: Map[NodeId, Seq[NodeId]]) : Map[NodeId,NodeLabel] = {
      val withoutRedundant = dropRedundantPlaceholders(nodeLabels)
      val order = breadthFirstTraversal(withoutRedundant, root, children)
      placeholderNormalForm(withoutRedundant, order)
    }

    private def dropRedundantPlaceholders(nodeLabels: Map[NodeId,NodeLabel]): Map[NodeId,NodeLabel] = {
      val dropper = SubstitutionUpdate.redundantPlaceholderDropper(nodeLabels.values)
      nodeLabels.map{
        case (id, lbl) => (id, lbl.update(dropper))
      }
    }

    /**
      * Ensure that placeholder vars are named ?1, ?2... without gap and increasing with the distance to the root.
      * This leads to a normal form for the node labels. (Not necessarily a normal form for the trees themselves, because the node IDs themselves may have gaps.)
      */
    def placeholderNormalForm(nodeLabels: Map[NodeId,NodeLabel], nodeLabelOrder: Seq[NodeLabel]): Map[NodeId,NodeLabel] = {
      val updateF = NodeLabel.labelsToPlaceholderNormalForm(nodeLabelOrder)
      nodeLabels.map{
        case (id, lbl) => (id, lbl.update(updateF))
      }
    }

    def breadthFirstTraversal(nodeLabels: Map[NodeId,NodeLabel], root: NodeId, children: Map[NodeId, Seq[NodeId]]): Stream[NodeLabel] = {
      def traverseByLayer(nodes: Seq[NodeId]): Stream[NodeLabel] = {
        if (nodes.isEmpty) {
          Stream.empty
        }
        else {
          val childLayer = nodes.flatMap(n => children(n))
          nodes.toStream.map(nodeLabels(_)) ++ traverseByLayer(childLayer)
        }
      }
      traverseByLayer(Seq(root))
    }

  }

}
