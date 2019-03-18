package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RichSid}

object EntailmentContextComposition extends HarrshLogging {

  def apply(sid: RichSid, fst: EntailmentContext, snd: EntailmentContext, constraints: VarConstraints): Stream[(EntailmentContext, ConstraintUpdater)] = {
    for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(fst, snd)
      _ = logger.debug(s"Trying to compose on root ${t1.root} and leaf $n2")
      if !doubleAlloc(t1.root, n2, constraints.usage)
      //propagateUnification = unification(t1.root, n2)
      (speculativeEqs, nonspeculativeEqs) = unificationEqualities(t1.root, n2)
      if speculativeEqs.isEmpty
      // TODO: Streamline this into a single update step? Nontrivial because of possible transitive equalities between placeholders
      instantiationPreUnification = instantiate(t2, n2, t1)
      nonspeculativeUpdate = PureAtomUpdate(nonspeculativeEqs, fst.classes ++ snd.classes)
      instantiationAferUnification = instantiationPreUnification.updateSubst(nonspeculativeUpdate)
      dropperUpdate = DropperUpdate(instantiationAferUnification.redundantPlaceholders)
      instantiation = instantiationAferUnification.updateSubst(dropperUpdate)
      _ = assert(!instantiation.calls.contains(instantiation.root))
    } yield (instantiation, nonspeculativeUpdate)
  }

  private def unificationEqualities(fst: ContextPredCall, snd: ContextPredCall): (Seq[PureAtom],Seq[PureAtom]) = {
    val pairs = for {
      (v1, v2) <- fst.subst.toSeq zip snd.subst.toSeq
      // If we're equating two vars which aren't already known to be equal...
      if v1 != v2
    } yield (v1, v2)

    val (speculativeUnif, nonspeculativeUnif) = pairs.partition {
      case (v1, v2) =>
        v1.exists(!PlaceholderVar.isPlaceholder(_)) && v2.exists(!PlaceholderVar.isPlaceholder(_))
    }

    def mkAtom(pair: (Set[Var], Set[Var])) = PureAtom(pair._1.head, pair._2.head, isEquality = true)
    val res@(speculative, nonspeculative) = (speculativeUnif map mkAtom, nonspeculativeUnif map mkAtom)
    if (speculative.nonEmpty) {
      logger.debug(s"Unification of $fst and $snd imposes new equalities $res")
    }
    res
  }

//  private def unification(fst: ContextPredCall, snd: ContextPredCall): SubstitutionUpdate = {
//    val zipped: Seq[(Set[Var], Set[Var])] = fst.subst.toSeq zip snd.subst.toSeq
//    val unificationResult = zipped.map(pair => pair._1.union(pair._2))
//    SubstitutionUpdate.fromSetsOfEqualVars(unificationResult)
//  }

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

//  def instantiate(toInstantiate: EntailmentContext, abstractLeaf: ContextPredCall, instantiation: EntailmentContext, propagateUnification: SubstitutionUpdate): EntailmentContext = {
//    // When executing more than one composition step for a pair of decompositions,
//    // the following assertion does not necessarily hold from the second composition step onwards
//    // That's because we propagate any unification that happens in a context composition into all later composition
//    // steps, thus potentially introducing placeholders from earlier contexts into later contexts
//    // TODO Currently, the composition code should no longer depend in any way on non-overlapping placeholders, but I'll keep this assertion here for the time being as a reminder to revisit the invariants that have to hold in composition.
////    assert(EntailmentContext.haveDisjointPlaceholders(toInstantiate, instantiation),
////      s"Overlapping placeholders between $toInstantiate and $instantiation")
//
//    val newRoot = toInstantiate.root.update(propagateUnification)
//    val allLeaves = (toInstantiate.calls - abstractLeaf) ++ instantiation.calls
//    val newLeaves = allLeaves.map(_.update(propagateUnification))
//    EntailmentContext(newRoot, newLeaves)
//  }

}
