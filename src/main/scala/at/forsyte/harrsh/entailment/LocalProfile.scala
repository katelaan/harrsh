package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.{FreeVar, NullConst, Renaming, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

sealed trait LocalProfile {
  def areDefined: Boolean = this match {
    case NoLocalProfile => true
    case ProfileOfLocalAtoms(ets) => ets.nonEmpty
  }
}

case object NoLocalProfile extends LocalProfile
case class ProfileOfLocalAtoms(profile: Set[ContextDecomposition]) extends LocalProfile

object LocalProfile extends HarrshLogging {

  def apply(lab: SymbolicHeap, sid: SID): LocalProfile = {
    logger.debug(s"Will compute profile for local allocation of $lab")
    if (lab.pointers.isEmpty) {
      if (lab.pure.nonEmpty) {
        // TODO: It seems like we could get support for empty base rules on the LHS by returning a profile consisting of a zero-element decomposition with an ensured set of pure constraints matching those in lab. Is that correct?
        throw new IllegalArgumentException(s"Can't process symbolic heap without allocation but with pure constraints: $lab")
      } else {
        logger.debug(s"No pointers/pure atoms in rule => don't compute local profile")
        NoLocalProfile
      }
    } else {
      assert(lab.pointers.size == 1)
      val res = localAllocToDecomps(lab, sid)
      logger.debug(s"Profile for local allocation of $lab:\n${res.mkString("\n")}")
      ProfileOfLocalAtoms(res)
    }
  }

  def localAllocToDecomps(lhs: SymbolicHeap, sid: SID) : Set[ContextDecomposition] = {
    logger.debug(s"Local allocation: Creating (${sid.description})-profile from $lhs")

    for {
      pred <- sid.preds.toSet[Predicate]
      rule <- pred.rules
      if rule.hasPointer
      decomp <- mkDecomp(sid, pred, lhs, rule.body)
    } yield decomp
  }

  private def mkDecomp(sid: SID, predicate: Predicate, lhs: SymbolicHeap, rhs: SymbolicHeap): Option[ContextDecomposition] = {
    if (lhs.pointers.head.to.size != rhs.pointers.head.to.size) {
      None
    } else {
      for {
        ctx <- computeContextFromHeaps(sid, predicate, lhs, rhs)
        decomp = ContextDecomposition(Seq(ctx))
        // TODO: If we want to relax the assumption about rootedness, this call has to go
        if allRootParamsUsed(decomp, sid)
        if hasNamesForUsedParams(decomp, sid)
      } yield decomp
    }
  }

  private def allRootParamsUsed(decomp: ContextDecomposition, sid: SID): Boolean = {
    val rootsUsed = for {
      ctx <- decomp.parts.toStream
      node <- ctx.calls
      usage = ctx.usageInfoOfNode(node)
      rootParam <- node.pred.rootParam
      ix = node.freeVarSeq.indexOf(rootParam)
      // If a root parameter is a placeholder (i.e., not a proper free variable) it need not be used
      // TODO [Entailment cleanup] This only makes sense for SIDs that are *not* in the btw fragment, namely SIDs that define disconnected models, see e.g. examples/entailment/various/list-segments-different-order.hrs. We could drop this or explicitly switch it on if we detect that it's necessary.
      if node.subst.toSeq(ix) exists (!PlaceholderVar.isPlaceholder(_))
    } yield usage(ix).isUsed
    rootsUsed.forall(b => b)
  }

  private def hasNamesForUsedParams(decomp: ContextDecomposition, sid: SID): Boolean = {
    // Everything that's used has a name
    val enoughNamesByNodeAndParam = for {
      ctx <- decomp.parts.toStream
      // TODO: Do we have to consider the root here?
      node <- ctx.calls + ctx.root
      usageByVar = (node.subst.toSeq, ctx.usageInfoOfNode(node)).zipped
      (substVars, usg) <- usageByVar
      res = !usg.isUsed || substVars.exists(!PlaceholderVar.isPlaceholder(_))
      _ = {
        if (!res) logger.debug(s"Not enough names for $node: $substVars is not marked as used")
      }
    } yield res
    enoughNamesByNodeAndParam.forall(b => b)
  }

  private def computeContextFromHeaps(sid: SID, predicate: Predicate, lhs: SymbolicHeap, rhs: SymbolicHeap): Option[EntailmentContext] = {
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
    val pure = pureConstraints(lhs, renamedRhs, pureConstraintsAbducedByMatching)

    // 5.) If that's consistent, return the corresponding context, converting it to normalform first
    contextFromConstraints(renamedRhs, newParams, pure, sid, predicate)
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

  private def pureConstraints(lhs: SymbolicHeap, renamedRhs: SymbolicHeap, pureConstraintsAbducedByMatching: Set[PureAtom]): PureConstraintTracker = {
    val rhsToEnsure = Closure.fromSH(renamedRhs).asSetOfAtoms ++ pureConstraintsAbducedByMatching
    val lhsEnsuredConstraints = Closure.fromSH(lhs).asSetOfAtoms
    val rhsMissing = rhsToEnsure -- lhsEnsuredConstraints
    PureConstraintTracker(lhsEnsuredConstraints, rhsMissing)
  }

  private def contextFromConstraints(renamedRhs: SymbolicHeap, rootParams: Seq[Var], pure: PureConstraintTracker, sid: SID, predicate: Predicate): Option[EntailmentContext] = {
    if (pure.isConsistent) {
      contextFromConsistentConstraints(renamedRhs, rootParams, pure, sid, predicate)
    }
    else {
      // Inconsistent pure constraints => Discard matching
      None
    }
  }

  private def contextFromConsistentConstraints(renamedRhs: SymbolicHeap, rootParams: Seq[Var], pure: PureConstraintTracker, sid: SID, predicate: Predicate): Option[EntailmentContext] = {
    val allEnsured = pure.closure
    val toSet = (v: Var) => allEnsured.getEquivalenceClass(v)
    val toSubst = (vs: Seq[Var]) => Substitution(vs map toSet)

    val root = PredicateNodeLabel(predicate, toSubst(rootParams))
    val leaves = renamedRhs.predCalls map {
      case PredCall(name, args) => PredicateNodeLabel(sid(name), toSubst(args))
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
      val usageInfo: VarUsageByLabel = renamedRhs.allNonNullVars.map(v => (toSet(v), varToUsageInfo(v))).toMap
      Some(EntailmentContext(root, leavesAsSet, usageInfo, pure, convertToNormalform = true))
    } else {
      // Otherwise we've set the parameters for two calls to be equal,
      // which always corresponds to an unsatisfiable unfolding for SIDs that satisfy progress.
      // We must not make it satisfiable by accidentally identifying these calls in the set conversion
      None
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
