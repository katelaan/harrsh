package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RichSid}
import at.forsyte.harrsh.util.{HarrshCache, UnboundedCache}

object EntailmentContextComposition extends HarrshLogging {

  private type From = (RichSid, EntailmentContext, EntailmentContext, VarConstraints)
  private type Key = (EntailmentContext, EntailmentContext, VarConstraints)
  private type Value = Stream[(EntailmentContext, VarConstraints, ConstraintUpdater)]

  private val ctxCompositionCache: HarrshCache[From, Value] = new UnboundedCache[From, Key, Value](
    "Context Composition Cache",
    tuple => (tuple._2, tuple._3, tuple._4),
    from => compose(from._1, from._2, from._3, from._4)
  )

  def apply(sid: RichSid, fst: EntailmentContext, snd: EntailmentContext, constraints: VarConstraints): Stream[(EntailmentContext, VarConstraints, ConstraintUpdater)] = {
    ctxCompositionCache((sid,fst,snd,constraints))
  }

  private def compose(sid: RichSid, fst: EntailmentContext, snd: EntailmentContext, constraints: VarConstraints): Stream[(EntailmentContext, VarConstraints, ConstraintUpdater)] = {
    for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(fst, snd)
      _ = logger.debug(s"Trying to compose on root ${t1.root} and leaf $n2")
      if !doubleAlloc(t1.root, n2, constraints.usage)
      eqs = unificationEqualities(t1.root, n2)
      // TODO: Abort if we need to speculate something about bound vars
      // TODO: Streamline this into a single update step? Nontrivial because of possible transitive equalities between placeholders

      instantiationPreUnification = instantiate(t2, n2, t1)
      // Step 1: Aply equalities imposed by unification
      speculativeUpdate = SpeculativeUpdate(eqs, fst.classes ++ snd.classes)
      intermediateConstraints <- speculativeUpdate(constraints)
      intermediateInstantiation = instantiationPreUnification.updateSubst(speculativeUpdate)

      // Step 2: Get rid of superfluous placeholders
      dropperUpdate = DropperUpdate(intermediateInstantiation.redundantPlaceholders)
      instantiation = intermediateInstantiation.updateSubst(dropperUpdate)
      updatedConstraints <- dropperUpdate(intermediateConstraints)

      // Avoid speculation that would lead to unfounded proofs
      if !instantiation.calls.contains(instantiation.root)
//      _ = assert(!instantiation.calls.contains(instantiation.root),
//        s"Fully circular context $instantiation when assuming constraints $updatedConstraints"
//      )

      //_ = logger.debug(s"Constraints:\n0. $constraints\n1. $intermediateConstraints\n2. $updatedConstraints")

      fullUpdate = ChainedUpdater(speculativeUpdate, dropperUpdate)
    } yield (instantiation, updatedConstraints, fullUpdate)
  }

  private def unificationEqualities(fst: ContextPredCall, snd: ContextPredCall): Seq[PureAtom] = {
    for {
      (v1, v2) <- fst.subst.toSeq zip snd.subst.toSeq
      // If we're equating two vars which aren't already known to be equal...
      if v1 != v2
    } yield PureAtom(v1.head, v2.head, isEquality = true)
  }

  private case class CompositionInterface(ctxToEmbed: EntailmentContext, embeddingTarget: EntailmentContext, leafToReplaceInEmbedding: ContextPredCall)

  private def compositionCandidates(fst: EntailmentContext, snd: EntailmentContext): Stream[CompositionInterface] = {
    for {
      (ctxWithRoot, ctxWithAbstractLeaf) <- Stream((fst,snd), (snd,fst))
      root = ctxWithRoot.root
      abstractLeaf <- ctxWithAbstractLeaf.calls
      // Only consider for composition if the labeling predicates are the same
      if root.pred == abstractLeaf.pred
    } yield CompositionInterface(ctxWithRoot, ctxWithAbstractLeaf, abstractLeaf)
  }

  private def doubleAlloc(fst: ContextPredCall, snd: ContextPredCall, usage: VarUsageByLabel): Boolean = {
    logger.debug(s"Checking if matching $fst and $snd implies double allocation wrt usage $usage")
    assert(fst.pred == snd.pred)
    assert(fst.freeVarSeq == snd.freeVarSeq)
    (fst.subst.toSeq zip snd.subst.toSeq) exists {
      case (v1, v2) => {
        val res = usage(v1) == VarAllocated && usage(v2) == VarAllocated && v1.intersect(v2).isEmpty
        if (res) {
          logger.debug(s"Can't compose $fst and $snd: Cannot unify $v1 with $v2 because of double allocation wrt usage $usage.")
        }
        res
      }
    }
  }

  def instantiate(toInstantiate: EntailmentContext, abstractLeaf: ContextPredCall, instantiation: EntailmentContext): EntailmentContext = {
    val newRoot = toInstantiate.root
    val newLeaves = (toInstantiate.calls - abstractLeaf) ++ instantiation.calls
    EntailmentContext(newRoot, newLeaves)
  }

}
