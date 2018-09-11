package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.FreeVar

import scala.annotation.tailrec

trait CanCompose[A] {

  def makeDisjoint(fst: A, snd: A): (A, A)

  def root(a : A): NodeLabel

  def abstractLeaves(a: A): Set[AbstractLeafNodeLabel]

  def usageInfo(a: A, n: NodeLabel): VarUsageInfo

  def tryInstantiate(toInstantiate: A, abstractLeaf: AbstractLeafNodeLabel, instantiation: A, unification: Unification): Option[A]

}

object CanCompose extends HarrshLogging {
  def apply[A](implicit cc: CanCompose[A]): CanCompose[A] = cc

  implicit class CanComposeOps[A: CanCompose](fst: A) {
    def compose(snd: A) : Option[A] = CanCompose.compose(fst, snd)
  }

  def compose[A: CanCompose](fst: A, snd: A): Option[A] = {
    val cc = CanCompose[A]
    logger.debug(s"Will try to compose $fst with $snd.")

    val shifted@(shiftedFst, shiftedSnd) = cc.makeDisjoint(fst, snd)
    logger.debug(s"After shifting: $shifted")

    (for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(shiftedFst, shiftedSnd)
      unification <- tryUnify(t1, cc.root(t1).asInstanceOf[RuleNodeLabel], t2, n2)
      // Compose using the unification. (This can fail in case the unification leads to double allocation)
      instantiation <- cc.tryInstantiate(t2, n2, t1, unification)
    } yield instantiation).headOption
  }

  private def tryUnify[A: CanCompose](a1: A, n1: RuleNodeLabel, a2: A, n2: AbstractLeafNodeLabel): Option[Unification] = {
    logger.debug(s"Will try to unify $n1 with $n2")
    val cc = CanCompose[A]
    assert(cc.root(a1) == n1)
    assert(cc.abstractLeaves(a2).contains(n2))
    assert(n1.freeVarSeq == n2.freeVarSeq)
    val (n1usage, n2usage) = (cc.usageInfo(a1, n1), cc.usageInfo(a2, n2))

    // Sanity check: The root parameter of the predicate is marked as used in both nodes
    n1.pred.rootParam foreach {
      param =>
        val ix = n1.freeVarSeq.indexOf(param)
        assert(n1usage(ix).isUsed && n2usage(ix).isUsed,
          s"Root parameter ${n1.pred.rootParam.get} isn't marked as used in at least one of $n1 and $n2 (usage info: ${n1usage.mkString(",")}; ${n2usage.mkString(",")})")
    }

    val unifiableParams: Seq[Boolean] = (n1.freeVarSeq, n1usage, n2usage).zipped.toSeq.map{
      tuple: (FreeVar, VarUsage, VarUsage) => tuple match {
        case (_, VarUsage.Allocated, VarUsage.Allocated) =>
          // Double allocation
          false
        case (_, VarUsage.Unused, _) =>
          // Only used in one of the objects => Don't need to have a name in both
          true
        case (_, _, VarUsage.Unused) =>
          // Only used in one of the objects => Don't need to have a name in both
          true
        case (v, used1, used2) =>
          assert(used1.isUsed && used2.isUsed)
          // Used in both => Need a common name
          (n1.rootParamSubst.get intersect n2.rootParamSubst.get).nonEmpty
      }
    }

    for {
      (v, unifiable) <- n1.freeVarSeq.zip(unifiableParams)
      if !unifiable
    } {
      logger.debug(s"Can't unify $v (among FVs ${n1.freeVarSeq}) in $n1 and $n2. (Shared non-placeholder name required but not present.)")
    }

    if (unifiableParams.forall(b => b)) {
      Some((n1.subst.toSeq, n2.subst.toSeq).zipped.map(_ union _))
    } else {
      None
    }
  }

  /* Note: Since we're using NodeLabels rather than e.g. NodeIDs here, we do not consider all compositions in the corner
     case that there are two leaves with the exact same node label. This doesn't matter for correctness though, since in
     such cases, it simply doesn't make a difference which leaf we replace, so it's not necessary to keep both
     candidates around.

     Note further that under the assumption that even base rules allocate memory, such objects will anyway always
     represent double allocation (same node label implies same root), so they should anyway be discarded.
   */
  case class CompositionInterface[A](treeToEmbed: A, embeddingTarget: A, leafToReplaceInEmbedding: AbstractLeafNodeLabel)

  private def compositionCandidates[A: CanCompose](fst: A, snd: A): Stream[CompositionInterface[A]] = {
    val cc = CanCompose[A]
    for {
      (treeWithRoot, treeWithAbstractLeaf) <- Stream((fst,snd), (snd,fst))
      root = cc.root(treeWithRoot)
      if !root.isAbstractLeaf // Can't compose trivial trees consisting of nothing but an abstract root
      abstractLeaf <- cc.abstractLeaves(treeWithAbstractLeaf)
      // Only consider for composition if the labeling predicates are the same
      if root.pred == abstractLeaf.pred
    } yield CompositionInterface(treeWithRoot, treeWithAbstractLeaf, abstractLeaf)
  }

  /**
    * Execute as many composition steps as possible on `as`, returning a result where no further composition steps are possible.
    */
  def composeAll[A: CanCompose](as: Seq[A]): Seq[A] = sweepingMerge(Seq.empty, as)

  @tailrec private def sweepingMerge[A: CanCompose](processed: Seq[A], unprocessed: Seq[A]): Seq[A] = {
    if (unprocessed.isEmpty) {
      processed
    } else {
      tryMerge(unprocessed.head, unprocessed.tail) match {
        case Some((merged, other)) => sweepingMerge(processed, merged +: other)
        case None => sweepingMerge(processed :+ unprocessed.head, unprocessed.tail)
      }
    }
  }

  private def tryMerge[A: CanCompose](fst: A, other: Seq[A]): Option[(A, Seq[A])] = {
    (for {
      candidate <- other.toStream
      composed <- CanCompose.compose(fst, candidate)
    } yield (composed, other.filter(_ != candidate))).headOption
  }

}
