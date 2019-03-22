package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.{FreeVar, NullConst, Renaming, Var}
import at.forsyte.harrsh.seplog.inductive._

object LocalProfile extends HarrshLogging {

  def apply(lab: SymbolicHeap, sid: RichSid): EntailmentProfile = {
    logger.debug(s"Will compute profile for local allocation of $lab")
    val params = varsInLocalAllocation(lab)
    // TODO: Use sharedConstraints as a starting point in constructing the constraints of the decomps
    val sharedConstraints = ensuredLocalConstraints(params, lab)
    val decomps = decompsOfLocalAllocation(params, lab, sid).filter(_.isConsistentWithFocus(sid))

    if (decomps.isEmpty) {
      logger.debug(s"No decompositions in local profile => Will record constraints $sharedConstraints")
      ProfileOfNondecomposableModels(sharedConstraints, params)
    } else {
      logger.debug(s"Decompositions in local profile of $lab:\n${decomps.mkString("\n")}")
      ProfileOfDecomps(decomps, sharedConstraints, params)
    }
  }

  private def usageLookupForHeap(lab: SymbolicHeap): Set[Var] => VarUsage = {
    val ptrSet = lab.pointers.toSet[PointsTo]
    val alloced: Set[Var] = lab.pointers.map(_.from).toSet
    val refed: Set[Var] = ptrSet.flatMap(_.to)
      vs: Set[Var] =>
        if (vs.exists(alloced)) VarAllocated
        else if (vs.exists(refed)) VarReferenced
        else VarUnused
  }

  private def ensuredLocalConstraints(params: Seq[Var], lab: SymbolicHeap): VarConstraints = {
    val initialConstraints = VarConstraints.fromAtoms(params, Closure.fromSH(lab).asSetOfAtoms)
    val usageOf = usageLookupForHeap(lab)
    val usage = initialConstraints.usage map (pair => (pair._1, usageOf(pair._1)))
    initialConstraints.copy(usage = usage)
  }

  private def decompsOfLocalAllocation(params: Seq[Var], lab: SymbolicHeap, sid: RichSid): Set[ContextDecomposition] = {
    if (lab.pointers.isEmpty) {
      Set(decompIfNoAllocation(params, lab))
    } else {
      assert(lab.pointers.size == 1)
      decompsOfNonemptyLocalAllocation(lab, sid)
    }
  }

  private def varsInLocalAllocation(lab: SymbolicHeap): Seq[Var] = {
    val varsInPtrs = lab.pointers.flatMap(_.getNonNullVars)
    val varsInPure = lab.pure.flatMap(_.getNonNullVars)
    (varsInPtrs ++ varsInPure).distinct
  }

  private def decompIfNoAllocation(params: Seq[Var], lab: SymbolicHeap): ContextDecomposition = {
    logger.debug(s"No pointers in rule => only pure constraint in local profile")
    ContextDecomposition(Set.empty, VarConstraints.fromAtoms(params, lab.pure))
  }

  def decompsOfNonemptyLocalAllocation(lhs: SymbolicHeap, sid: RichSid) : Set[ContextDecomposition] = {
    logger.debug(s"Local allocation: Creating (${sid.description})-profile from $lhs")

    for {
      pred <- sid.preds.toSet[Predicate]
      rule <- pred.rules
      if rule.hasPointer
      decomp <- mkDecomp(sid, pred, lhs, rule.body)
    } yield decomp
  }

  private def mkDecomp(sid: RichSid, predicate: Predicate, lhs: SymbolicHeap, rhs: SymbolicHeap): Option[ContextDecomposition] = {
    if (lhs.pointers.head.to.size != rhs.pointers.head.to.size) {
      None
    } else {
      for {
        decomp <- computeDecompFromHeaps(sid, predicate, lhs, rhs)
        // TODO: If we want to relax the assumption about rootedness, this call has to go
        if allRootParamsUsed(decomp, sid)
        if hasNamesForUsedParams(decomp, sid)
      } yield decomp
    }
  }

  private def allRootParamsUsed(decomp: ContextDecomposition, sid: RichSid): Boolean = {
    val rootsUsed = for {
      ctx <- decomp.parts.toStream
      call <- ctx.calls
      usage = decomp.usageInfoOfCall(call)
      rootParam <- sid.roots.get(call.pred.head)
      ix = call.freeVarSeq.indexOf(rootParam)
      // If a root parameter is a placeholder (i.e., not a proper free variable) it need not be used
      // TODO [Entailment cleanup] This only makes sense for SIDs that are *not* in the btw fragment, namely SIDs that define disconnected models, see e.g. examples/entailment/various/list-segments-different-order.hrs. We could drop this or explicitly switch it on if we detect that it's necessary.
      if call.subst.toSeq(ix) exists (!PlaceholderVar.isPlaceholder(_))
    } yield usage(ix).isUsed
    val res = rootsUsed.forall(b => b)
    if (!res) logger.debug("Discarding decomposition with unused root parameters: " + decomp)
    res
  }

  private def hasNamesForUsedParams(decomp: ContextDecomposition, sid: RichSid): Boolean = {
    // Everything that's used has a name
    val enoughNamesByNodeAndParam = for {
      ctx <- decomp.parts.toStream
      // TODO: Do we have to consider the root here?
      call <- ctx.calls + ctx.root
      usageByVar = (call.subst.toSeq, decomp.usageInfoOfCall(call)).zipped
      (substVars, usg) <- usageByVar
      res = !usg.isUsed || substVars.exists(!PlaceholderVar.isPlaceholder(_))
      _ = {
        if (!res) logger.debug(s"Not enough names for $call: $substVars is not marked as used")
      }
    } yield res
    val res = enoughNamesByNodeAndParam.forall(b => b)
    if (!res) logger.debug("Discarding decomposition without names for used params: " + decomp)
    res
  }

  private def computeDecompFromHeaps(sid: RichSid, predicate: Predicate, lhs: SymbolicHeap, rhs: SymbolicHeap): Option[ContextDecomposition] = {
    logger.debug(s"Trying to construct context for $lhs |= $rhs")

    // 1.) In the rule body, replace all bound vars and all FVs that don't occur by placeholders
    val closure = Closure.fromSH(rhs)
    val withPlaceholders = introducePlaceholders(rhs, closure)
    logger.debug(s"RHS after placeholder introduction: $withPlaceholders")

    // 2.) Perform a speculative matching of the LHS against the thus-modified RHS
    val MatchResult(renamingTargets, pureConstraintsAbducedByMatching) = matchPointers(lhs, withPlaceholders, closure)

    // 3.) Rename free vars in the rule body to LHS vars based on the match result
    val ruleArity = rhs.freeVars.size
    val (renamedRhs, newParams) = renameRuleBody(ruleArity, withPlaceholders, renamingTargets)

    // 4.) Compute pure constraints enforced by the LHS, the RHS and the specific matching
    val (ensured, missing) = pureConstraints(lhs, renamedRhs, pureConstraintsAbducedByMatching)

    // 5.) If that's consistent, return the corresponding context, converting it to normalform first
    contextFromConstraints(renamedRhs, newParams, ensured, missing, sid, predicate)
  }

  private def introducePlaceholders(rhs: SymbolicHeap, rhsClosure: Closure): SymbolicHeap = {
    // 1.) Replace all FVs that don't occur in the RHS pointer by placeholders
    val usedFvs = Var.freeNonNullVars(rhs.pointers.head.args) flatMap (rhsClosure.getEquivalenceClass(_))
    val unused = rhs.freeVars.filterNot(usedFvs.contains)
    // 2.) Replace all bound vars in body by placeholder vars
    val bvs = rhs.boundVars
    PlaceholderVar.replaceVarsWithPlaceholders(rhs, unused ++ bvs)
  }

  private def renameRuleBody(ruleArity: Int, rhsWithPlaceholders: SymbolicHeap, renamingTargets: Seq[Var]) = {
    assert(rhsWithPlaceholders.freeVars.size == renamingTargets.size)
    val renaming = Renaming.fromPairs(rhsWithPlaceholders.freeVars zip renamingTargets)
    // Note: Targets may contain null, so filter for non-null FVs
    val renamedRhs = rhsWithPlaceholders.rename(renaming, overrideFreeVars = Some(Var.freeNonNullVars(renamingTargets)))
    val newParams = rhsWithPlaceholders.freeVars.take(ruleArity).map(renaming(_))
    (renamedRhs, newParams)
  }

  private def pureConstraints(lhs: SymbolicHeap, renamedRhs: SymbolicHeap, pureConstraintsAbducedByMatching: Set[PureAtom]): (Set[PureAtom], Set[PureAtom]) = {
    val rhsToEnsure = Closure.fromSH(renamedRhs).asSetOfAtoms ++ pureConstraintsAbducedByMatching
    val lhsEnsuredConstraints = Closure.fromSH(lhs).asSetOfAtoms
    val rhsMissing = rhsToEnsure -- lhsEnsuredConstraints
    logger.debug(s"Pure constraints: Ensured are $lhsEnsuredConstraints; missing are: $rhsMissing")
    (lhsEnsuredConstraints, rhsMissing)
  }

  private def contextFromConstraints(renamedRhs: SymbolicHeap, rootParams: Seq[Var], ensured: Set[PureAtom], missing: Set[PureAtom], sid: RichSid, predicate: Predicate): Option[ContextDecomposition] = {
    if (ensured.forall(_.isConsistent) && missing.forall(_.isConsistent)) {
      contextFromConsistentConstraints(renamedRhs, rootParams, ensured, missing, sid, predicate)
    }
    else {
      // Inconsistent pure constraints => Discard matching
      None
    }
  }

  private def contextFromConsistentConstraints(renamedRhs: SymbolicHeap, rootParams: Seq[Var], ensured: Set[PureAtom], missing: Set[PureAtom], sid: RichSid, predicate: Predicate): Option[ContextDecomposition] = {
    // TODO: Clean up this monster of a method
    assert(rootParams.toSet subsetOf renamedRhs.allVars)
    assert(ensured.intersect(missing).isEmpty)
    val vars = renamedRhs.allVars ++ ensured.flatMap(_.getVars) ++ missing.flatMap(_.getVars)
    val initialConstraints = VarConstraints.fromAtoms(vars, ensured)
    val addSpeculation = SpeculativeUpdate(missing, initialConstraints.classes)
    addSpeculation(initialConstraints).flatMap { constraintsWithoutUsage =>
      val toSet = (v: Var) => constraintsWithoutUsage.classOf(v)
      val toSubst = (vs: Seq[Var]) => Substitution(vs map toSet)

      val root = ContextPredCall(predicate, toSubst(rootParams))
      val leaves = renamedRhs.predCalls map {
        case PredCall(name, args) => ContextPredCall(sid(name), toSubst(args))
      }
      val leavesAsSet = leaves.toSet
      if (leavesAsSet.size == leaves.size) {
        // Compute usage
        val renamedPtr = renamedRhs.pointers.head
        val varToUsageInfo = (v: Var) => {
          if (toSet(v).contains(renamedPtr.from)) VarAllocated
          else if (renamedPtr.to.flatMap(toSet).contains(v)) VarReferenced
          else VarUnused
        }
        val usageInfo: VarUsageByLabel = vars.map(v => (toSet(v), varToUsageInfo(v))).toMap
        val constraints = constraintsWithoutUsage.copy(usage = usageInfo)

        val ctx = EntailmentContext(root, leavesAsSet)
        val decomp = ContextDecomposition(Set(ctx), constraints)
        Some(decomp.toPlaceholderNormalForm)
      } else {
        // Otherwise we've set the parameters for two calls to be equal,
        // which always corresponds to an unsatisfiable unfolding for SIDs that satisfy progress.
        // We must not make it satisfiable by accidentally identifying these calls in the set conversion
        None
      }
    }
  }

  private case class MatchResult(renamingTargets: Seq[Var], abducedPureConstraints: Set[PureAtom])

  private def matchPointers(lhs: SymbolicHeap, rhs: SymbolicHeap, rhsClosure: Closure): MatchResult = {
    // Map the RHS vars to the LHS vars based on the points-to assertion in RHS and LHS
    val rhsToLhsPointerMatching = rhsToLhsMap(rhs, lhs)
    logger.debug(s"Computed pointer matching: $rhsToLhsPointerMatching")
    // Execute the renaming based on the matching
    val renamingTargets = renameByMatching(rhs, rhsClosure, rhsToLhsPointerMatching)
    // Collect aliasing and non-null constraints that must hold for the matching to work
    val abducedPureConstraints = abduceConstraints(rhsToLhsPointerMatching)
    MatchResult(renamingTargets, abducedPureConstraints)
  }

  private def renameByMatching(rhs: SymbolicHeap, rhsClosure: Closure, rhsToLhsPointerMatching: Map[Var, Seq[Var]]): Seq[Var] = {
    val rhsToLhsMapper = rhsVarToLhsVar(rhsToLhsPointerMatching, rhsClosure)_
    rhs.freeVars map rhsToLhsMapper
  }

  private def rhsToLhsMap(rhs: SymbolicHeap, lhs: SymbolicHeap): Map[Var,Seq[Var]] = {
    //Rename free vars in body rhs according to lhs
    val rhsPto = rhs.pointers.head.args
    val lhsPto = lhs.pointers.head.args
    logger.debug(s"Trying to find renaming to match $lhsPto |= $rhsPto")
    val renamingTargets = NullConst +: lhs.freeVars // The vars that we can rename to. Only those that actually appear in the LHS pointer
    (rhsPto zip lhsPto).groupBy(_._1) map {
      case (rhsVar, tuples) => (rhsVar, tuples.map(_._2))
    }
  }

  private def rhsVarToLhsVar(rhsToLhsPointerMatching: Map[Var,Seq[Var]], rhsClosure: Closure)(fv: Var): Var = {
    rhsToLhsPointerMatching.get(fv) match {
      case Some(lhsV) =>
        if (lhsV.exists(_.isFreeNonNull)) {
          // All free vars that occur in the pointer have a fixed mapping (matching the points-to assertions)
          // Since all variables in lhsV must alias, it's enough to consider one of those for the substitution
          lhsV.collect{
            case fv: FreeVar => fv
            case NullConst => NullConst
          }.head
        } else {
          // Quantified on the left, parameter on the right
          // Note: Perhaps counterintuitively, the rule can be matched regardless;
          // This is because we compute on the original LHS pointer, not on the "local" pointer that has additional fvs
          // TODO: This would become unreachable if we used the rename-compose-forget semantics as presented in the paper (where there simply aren't any bound variables when we compute the local profile, because we drop the quantifier prefix and only reintroduce it later via the forget)
          lhsV.head
        }
      case None =>
        // fv does not occur in the pointer
        if (PlaceholderVar.isPlaceholder(fv)) {
          // If it's a placeholder, it must remain unchanged
          fv
        } else {
          // If not, there must be aliasing effects that determine the mapping
          rhsClosure.getEquivalenceClass(fv).flatMap(rhsToLhsPointerMatching.get).headOption match {
            case Some(lhsVars) =>
              lhsVars.collect{
                case fv: FreeVar => fv
                case NullConst => NullConst
              }.head
            case None =>
              // All completely unused free vars should have been replaced by placeholders
              throw new IllegalStateException(s"This should be unreachable. There shouldn't be a nonplaceholder var that doesn't occur in the pointer, but there is $fv")
          }
        }
    }
  }

  private def abduceConstraints(rhsToLhsPointerMatching: Map[Var,Seq[Var]]): Set[PureAtom] = {
    aliasingConstraints(rhsToLhsPointerMatching) ++ nullConstraints(rhsToLhsPointerMatching)
  }

  private def nullConstraints(rhsToLhsPointerMatching: Map[Var,Seq[Var]]): Set[PureAtom] = {
    for {
      (rhs, lhsVs) <- rhsToLhsPointerMatching.find(_._1.isNull).toSet
      v <- lhsVs
      if !v.isNull
    } yield NullConst =:= v
  }

  private def aliasingConstraints(rhsToLhsPointerMatching: Map[Var,Seq[Var]]): Set[PureAtom] = {
    // If an RHS variable has to be mapped to multiple LHS variables, we must compute the corresponding aliasing constraints for that.
    // E.g. matching LHS x1 -> (x2, x3) against RHS x1 -> (x2, x2). This should be possible, but lead to aliasing constraint x2 = x3
    (for {
      (rhs, lhsVs) <- rhsToLhsPointerMatching
      if lhsVs.size > 1
      v1 <- lhsVs
      v2 <- lhsVs
      if v1 < v2
    } yield v1 =:= v2).toSet
  }

}
