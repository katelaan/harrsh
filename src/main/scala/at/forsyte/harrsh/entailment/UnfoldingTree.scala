package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.ToLatex

case class UnfoldingTree(nodeLabels: Map[NodeId,NodeLabel], root: NodeId, children: Map[NodeId, Seq[NodeId]]) extends HarrshLogging {

    import at.forsyte.harrsh.entailment.UnfoldingTree._

  lazy val nodes: Set[NodeId] = nodeLabels.keySet

  lazy val abstractLeaves: Set[NodeId] = nodes.filter(n => nodeLabels(n).isAbstractLeaf)

  def isAbstractLeaf(nodeId: NodeId): Boolean = nodeLabels(nodeId).isAbstractLeaf

  assert(nodes.contains(root))
  assert(children.keys forall nodes.contains)
  assert(children.values forall (_ forall nodes.contains))
  assert(abstractLeaves forall (children(_).isEmpty))

//  assert(nodes forall (n => children(n).nonEmpty
//    || isAbstractLeaf(n)
//    || (!isAbstractLeaf(n) && nodeLabels(n).asInstanceOf[RuleNodeLabel].rule.isBaseRule)),
//    s"Inconsistent unfolding tree $this")

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
    TreeInterface(nodeLabels(root), abstractLeaves map (nodeLabels(_).asInstanceOf[AbstractLeafNodeLabel]))
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
      children = ruleTreeChildTuples.toMap
    )
    val shiftedRuleTree = CanCompose[UnfoldingTree].avoidClashes(ruleTree, this)._1

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

  private def cleanUpPlaceholders: UnfoldingTree = {
    val withoutRedundant = dropRedundantPlaceholders
    withoutRedundant.closeGapsInPlaceholders
  }

  private def closeGapsInPlaceholders: UnfoldingTree = {
    val currentPlaceholders = placeholders.toSeq.sortBy(_.index)
    val newPlaceholders = (1 to currentPlaceholders.length) map (PlaceholderVar(_))
    val replacement = (currentPlaceholders, newPlaceholders).zipped.toMap
    val updateF: SubstitutionUpdate = {
      v => PlaceholderVar.fromVar(v) match {
        case Some(pv) => Set(replacement(pv).toFreeVar)
        case None => Set(v)
      }
    }
    updateSubst(updateF)
  }

  private def dropRedundantPlaceholders: UnfoldingTree = {
    def getRedundantVars(vs: Set[Var]): Set[Var] = {
      val (phs, nonPhs) = vs.partition(PlaceholderVar.isPlaceholder)
      if (nonPhs.nonEmpty) {
        // There is a proper free var in this equivalence class => discard all equivalent placeholders
        phs
      } else {
        // Keep only the smalles placeholder among multiple placeholders
        val typedPhs = phs map (ph => PlaceholderVar.fromVar(ph).get)
        phs - PlaceholderVar.min(typedPhs).toFreeVar
      }
    }
    val equivalenceClasses = extractVarEquivClasses(nodeLabels.values)
    val redundantVars = equivalenceClasses.flatMap(getRedundantVars)
    logger.debug(s"Reundant vars: $redundantVars")

    val updateF: SubstitutionUpdate = {
      v => if (redundantVars.contains(v)) Set.empty else Set(v)
    }
    updateSubst(updateF)
  }

  def extendLabeling(unification: Unification): UnfoldingTree = {
    val updateFn : SubstitutionUpdate = {
      v => unification.find(_.contains(v)).getOrElse(Set(v))
    }
    updateSubst(updateFn)
  }

  def instantiate(abstractLeaf: NodeId, replacingTree: UnfoldingTree, unification: Unification): UnfoldingTree = {
    assert(haveNoConflicts(this, replacingTree))
    logger.debug(s"Replacing $abstractLeaf in $this with $replacingTree")
    val thisExtended = this.extendLabeling(unification)
    val otherExtended = replacingTree.extendLabeling(unification)
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

    val combinedTree = UnfoldingTree(combinedNodeLabels, newRoot, combinedChildren)
    combinedTree.cleanUpPlaceholders
  }

  def placeholders: Set[PlaceholderVar] = nodeLabels.values.flatMap(_.placeholders).toSet

  def updateSubst(f: SubstitutionUpdate): UnfoldingTree = {
    val updatedLabels = nodeLabels.map {
      case (id,label) => (id, label.update(f))
    }
    copy(nodeLabels = updatedLabels)
  }

}

object UnfoldingTree extends HarrshLogging {

  implicit val treeToLatex: ToLatex[UnfoldingTree] = ForestsToLatex.treeToLatex
  implicit val canComposeUT: CanCompose[UnfoldingTree] = UnfoldingTreeComposition.canComposeUT

  private def getSubstOrDefault(subst: Option[Substitution], pred: Predicate): Substitution = {
    subst.getOrElse(Substitution.identity(pred.params))
  }

  def singleton(sid: SID, pred: Predicate, subst: Option[Substitution] = None): UnfoldingTree = {
    val label = AbstractLeafNodeLabel(pred, getSubstOrDefault(subst, pred))
    val id = NodeId.zero
    UnfoldingTree(Map(id -> label), id, Map(id -> Seq.empty))
  }

  def extractVarEquivClasses(labels: Iterable[NodeLabel]) : Set[Set[Var]] = Substitution.extractVarEquivClasses(labels map (_.subst))

  def haveNoConflicts(ut1: UnfoldingTree, ut2: UnfoldingTree) : Boolean = {
    (ut1.nodes intersect ut2.nodes).isEmpty && (ut1.placeholders intersect ut2.placeholders).isEmpty
  }

  def fromPredicate(sid: SID, pred: String, labeling: Substitution): UnfoldingTree = {
    val node = AbstractLeafNodeLabel(sid(pred), labeling)
    UnfoldingTree(Map(NodeId.zero -> node), NodeId.zero, Map.empty)
  }

  def placeholderNormalForm(ut: UnfoldingTree, nodeLabelOrder: Seq[NodeLabel]): UnfoldingTree = {
    val updateF = NodeLabel.labelsToPlaceholderNormalForm(nodeLabelOrder)
    ut.updateSubst(updateF)
  }

  /**
    * Ensure that placeholder vars are named ?1, ?2... without gap and increasing with the distance to the root.
    * This leads to a normal form for the node labels. (Not for the trees themselves, because the node IDs themselves may have gaps.)
    * @param ut
    * @return
    */
  def placeholderNormalForm(ut: UnfoldingTree): UnfoldingTree = {
    placeholderNormalForm(ut, breadthFirstTraversal(ut))
  }

  def breadthFirstTraversal(tree: UnfoldingTree): Stream[NodeLabel] = {
    def traverseByLayer(nodes: Seq[NodeId]): Stream[NodeLabel] = {
      if (nodes.isEmpty) {
        Stream.empty
      }
      else {
        val childLayer = nodes.flatMap(n => tree.children(n))
        nodes.toStream.map(tree.nodeLabels(_)) ++ traverseByLayer(childLayer)
      }
    }
    traverseByLayer(Seq(tree.root))
  }

}
