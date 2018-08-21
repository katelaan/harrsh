package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging

trait CanCompose[A] {

  def avoidClashes(fst: A, snd: A): (A, A)

  def root(a : A): NodeLabel

  def abstractLeaves(a: A): Set[AbstractLeafNodeLabel]

  def tryUnify(n1: NodeLabel, n2: NodeLabel): Option[Unification]

  def tryInstantiate(toInstantiate: A, abstractLeaf: NodeLabel, instantiation: A, unification: Unification): Option[A]

}

object CanCompose extends HarrshLogging {
  def apply[A](implicit cc: CanCompose[A]): CanCompose[A] = cc

  implicit class CanComposeOps[A: CanCompose](fst: A) {
    def compose(snd: A) : Option[A] = CanCompose.compose(fst, snd)
  }

  def compose[A: CanCompose](fst: A, snd: A): Option[A] = {
    val cc = CanCompose[A]
    logger.debug(s"Will try to compose $fst with $snd.")
    val shifted@(shiftedFst, shiftedSnd) = cc.avoidClashes(fst, snd)
    logger.debug(s"After shifting: $shifted")

    (for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(shiftedFst, shiftedSnd)
      unification <- cc.tryUnify(cc.root(t1), n2)
      // Compose using the unification. (This can fail in case the unification leads to double allocation)
      instantiation <- cc.tryInstantiate(t2, n2, t1, unification)
    } yield instantiation).headOption
  }

  /* Note: Since we're using NodeLabels rather than e.g. NodeIDs here, we do not consider all compositions in the corner
     case that there are two leaves with the exact same node label. This doesn't matter for correctness though, since in
     such cases, it simply doesn't make a difference which leaf we replace, so it's not necessary to keep both
     candidates around.

     Note further that under the assumption that even base rules allocate memory, such objects will anyway always
     represent double allocation (same node label implies same root), so they should anyway be discarded.
   */
  case class CompositionInterface[A](treeToEmbed: A, embeddingTarget: A, leafToReplaceInEmbedding: NodeLabel)

  private def compositionCandidates[A: CanCompose](fst: A, snd: A): Stream[CompositionInterface[A]] = {
    val cc = CanCompose[A]
    for {
      (treeWithRoot, treeWithAbstractLeaf) <- Stream((fst,snd), (snd,fst))
      root = cc.root(treeWithRoot)
      abstractLeaf <- cc.abstractLeaves(treeWithAbstractLeaf)
      // Only consider for composition if the labeling predicates are the same
      if root.pred == abstractLeaf.pred
    } yield CompositionInterface(treeWithRoot, treeWithAbstractLeaf, abstractLeaf)
  }

}
