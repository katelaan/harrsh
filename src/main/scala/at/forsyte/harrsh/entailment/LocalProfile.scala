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

  private def mkDecomp(sid: SID, predicate: Predicate, lhs: SymbolicHeap, rhs: SymbolicHeap): Set[ContextDecomposition] = {
    if (lhs.pointers.head.to.size != rhs.pointers.head.to.size) {
      Set.empty
    } else {
      for {
        decomp <- computeDecomps(sid, predicate, lhs, rhs)
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

  private def computeDecomps(sid: SID, predicate: Predicate, lhs: SymbolicHeap, rhs: SymbolicHeap): Set[ContextDecomposition] = {
    logger.debug(s"Trying to construct context for $lhs |= $rhs")
    // Idea:
    // 1.) Replace all FVs that don't occur in the RHS pointer by placeholders
    val closure = Closure.fromSH(rhs)
    val usedFvs = Var.freeNonNullVars(rhs.pointers.head.args) flatMap (closure.getEquivalenceClass(_))
    val unused = rhs.freeVars.filterNot(usedFvs.contains)
    // 2.) Replace all bound vars in body by placeholder vars
    val bvs = rhs.boundVars
    val withPlaceholders = PlaceholderVar.replaceVarsWithPlaceholders(rhs, unused ++ bvs)
    logger.debug(s"RHS after placeholder introduction: $withPlaceholders")

    val lhsEnsuredConstraints = Closure.fromSH(lhs).asSetOfAtoms

    val (renamings, aliasing) = compatibleRenamings(lhs, withPlaceholders, closure)

    // 3.) Rename free vars in body (rhs) according to lhs
    val ctxs = for {
      targets <- renamings
      _ = assert(withPlaceholders.freeVars.size == targets.size)
      renaming = Renaming.fromPairs(withPlaceholders.freeVars zip targets)
      // Note: Targets may contain null, so filter for non-null FVs
      renamedRhs = withPlaceholders.rename(renaming, Some(Var.freeNonNullVars(targets)))
      //renamedRhs = rhs.rename(renaming, Some(Var.freeNonNullVars(targets) ++ withPlaceholders.freeVars.drop(targets.size)))
      // 4.) Compute substitution, ensured and missing for symbolic heap in rule body
      rhsToEnsure = Closure.fromSH(renamedRhs).asSetOfAtoms ++ aliasing
      rhsMissing = rhsToEnsure -- lhsEnsuredConstraints
      pure = PureConstraintTracker(lhsEnsuredConstraints, rhsMissing)
      // 5.) If that's consistent, return the corresponding context, converting it to normalform first
      if pure.isConsistent
      allEnsured = pure.closure
      toSet = (v: Var) => allEnsured.getEquivalenceClass(v)
      toSubst = (vs: Seq[Var]) => Substitution(vs map toSet)
      //newParams = renamedRhs.freeVars.take(rhs.freeVars.size)
      newParams = withPlaceholders.freeVars.take(rhs.freeVars.size).map(renaming(_))
      root = PredicateNodeLabel(predicate, toSubst(newParams))
      leaves = renamedRhs.predCalls map {
        case PredCall(name, args) => PredicateNodeLabel(sid(name), toSubst(args))
      }
      leavesAsSet = leaves.toSet
      if leavesAsSet.size == leaves.size // Otherwise we've set the parameters for two calls to be equal,
      // which always corresponds to an unsatisfiable unfolding for SIDs that satisfy progress.
      // We must not make it satisfiable by accidentally identifying these calls in the set conversion

      // Compute usage
      renamedPtr = renamedRhs.pointers.head
      varToUsageInfo = (v: Var) => {
        if (toSet(v).contains(renamedPtr.from)) VarAllocated
        else if (renamedPtr.to.flatMap(toSet).contains(v)) VarReferenced
        else VarUnused
      }
      usageInfo: VarUsageByLabel = renamedRhs.allNonNullVars.map(v => (toSet(v), varToUsageInfo(v))).toMap
    } yield EntailmentContext(root, leavesAsSet, usageInfo, pure, convertToNormalform = true)
    (ctxs map (ctx => ContextDecomposition(Seq(ctx)))).toSet
  }

  private def compatibleRenamings(lhs: SymbolicHeap, rhs: SymbolicHeap, rhsClosure: Closure): (Stream[Seq[Var]],Set[PureAtom]) = {
    //Rename free vars in body rhs according to lhs
    val rhsPto = rhs.pointers.head.args
    val lhsPto = lhs.pointers.head.args
    logger.debug(s"Trying to find renaming to match $lhsPto |= $rhsPto")
    val renamingTargets = NullConst +: lhs.freeVars // The vars that we can rename to. Only those that actually appear in the LHS pointer
    val rhsToLhsPointerMatching: Map[Var, Seq[Var]] = (rhsPto zip lhsPto).groupBy(_._1) map {
      case (rhsVar, tuples) => (rhsVar, tuples.map(_._2))
    }
    logger.debug(s"Computed pointer matching: $rhsToLhsPointerMatching")
    val candidatesByFv: Seq[Seq[Var]] = rhs.freeVars map {
      fv =>
        rhsToLhsPointerMatching.get(fv) match {
          case Some(lhsV) =>
            if (lhsV.exists(_.isFreeNonNull)) {
              // All free vars that occur in the pointer have a fixed mapping (matching the points-to assertions)
              // Since all variables in lhsV must alias, it's enough to consider one of those for the substitution
              Seq(lhsV.collect{
                case fv: FreeVar => fv
                case NullConst => NullConst}.head)
            } else {
              // Quantified on the left, parameter on the right
              // Note: Perhaps counterintuitively, the rule can be matched regardless;
              // This is because we compute on the original LHS pointer, not on the "local" pointer that has additional fvs
              Seq(lhsV.head)
            }
          case None =>
            // fv does not occur in the pointer
            if (PlaceholderVar.isPlaceholder(fv)) {
              // If it's a placeholder, it must remain unchanged
              Seq(fv)
            } else {
              // If not, there must be aliasing effects that determine the mapping
              rhsClosure.getEquivalenceClass(fv).flatMap(rhsToLhsPointerMatching.get).headOption match {
                case Some(lhsVars) => lhsVars
                case None =>
                  // All completely unused free vars should have been replaced by placeholders
                  assert(false, s"This should be unreachable. There shouldn't be a nonplaceholder var that doesn't occur in the pointer, but there is $fv")
                  renamingTargets
              }
            }
        }
    }

    if (candidatesByFv.exists(_.isEmpty)) {
      logger.debug(s"One or more points-to arguments are not matchable in $lhsPto |= $rhsPto")
      (Stream.empty, Set.empty)
    } else {
      // If an RHS variable has to be mapped to multiple LHS variables, we must compute the corresponding aliasing constraints for that.
      // E.g. matching LHS x1 -> (x2, x3) against RHS x1 -> (x2, x2). This should be possible, but lead to aliasing constraint x2 = x3
      val aliasingConstraints = for {
        (rhs, lhsVs) <- rhsToLhsPointerMatching
        if lhsVs.size > 1
        v1 <- lhsVs
        v2 <- lhsVs
        if v1 < v2
      } yield v1 =:= v2

      val nullConstraints = for {
        (rhs, lhsVs) <- rhsToLhsPointerMatching
        if rhs.isNull
        v <- lhsVs
        if !v.isNull
        // TODO: For consistency with the original implementation, we must explicitly add the disequality between the allocated var and the null var here. Check if this is actually necessary (or null=v is sufficient) once we pass all tests
        allocedVs = rhsToLhsPointerMatching(rhsPto.head)
        constraint <- (NullConst =:= v) +: (allocedVs map (_ =/= v))
      } yield constraint //yield NullConst =:= v

      // TODO: This should always be empty or a singleton, i.e., it should be an option value not a stream, right?
      (Combinators.choices(candidatesByFv).toStream, aliasingConstraints.toSet ++ nullConstraints)
    }
  }

}
