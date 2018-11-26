package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{Closure, ConsistencyCheck}
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

  val UseNewLocalProfileComputation = true
  //val UseNewLocalProfileComputation = false

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
      val res = if (UseNewLocalProfileComputation) localAllocToDecompsNew(lab, sid) else localAllocToDecomps(lab, sid)
      logger.debug(s"Profile for local allocation of $lab:\n${res.mkString("\n")}")
      ProfileOfLocalAtoms(res)
    }
  }

  def localAllocToDecompsNew(lhs: SymbolicHeap, sid: SID) : Set[ContextDecomposition] = {
    logger.debug(s"Local allocation: Creating (${sid.description})-profile from $lhs")

    for {
      pred <- sid.preds.toSet[Predicate]
      rule <- pred.rules
      if rule.hasPointer
      decomp <- mkDecompNew(sid, pred, lhs, rule.body)
    } yield decomp
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

  private def mkDecompNew(sid: SID, predicate: Predicate, lhs: SymbolicHeap, rhs: SymbolicHeap): Set[ContextDecomposition] = {
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

  /*
   * OLD STUFF
   */

  private case class AnnotatedLocalHeap(sh: SymbolicHeap, pointerVarSeq: Seq[Var], ensuredPureConstraints: Set[PureAtom])

  def localAllocToDecomps(lhs: SymbolicHeap, sid: SID) : Set[ContextDecomposition] = {
    logger.debug(s"Local allocation: Creating (${sid.description})-profile from $lhs")
    val localLhs = annotatedLocalHeap(lhs)

    for {
      pred <- sid.preds.toSet[Predicate]
      rule <- pred.rules
      if rule.hasPointer
      decomp <- mkDecomp(sid, pred, rule, localLhs)
    } yield decomp
  }

  private def annotatedLocalHeap(lhs: SymbolicHeap): AnnotatedLocalHeap = {
    val localLhs = lhs.withoutCalls
    // Rename the variables that occur in the points-to assertion. (Only those, because we match only the pointers)
    val localPtr = localLhs.pointers.head
    val pointerVarSeq = localPtr.getNonNullVars.toSeq ++ localPtr.args.find(_ == NullConst)
    val ensuredPureConstraints = Closure.fromSH(localLhs).asSetOfAtoms
    AnnotatedLocalHeap(localLhs, pointerVarSeq, ensuredPureConstraints)
  }

  private def mkDecomp(sid: SID, pred: Predicate, rule: RuleBody, localLhs: AnnotatedLocalHeap): Set[ContextDecomposition] = {
    // Go over all ways to map a variable of the lhs to a variable of the rhs
    // FIXME: Don't brute force over assignments, but pick the suitable one(s) directly
    val body = rule.body
    val bodyVars = body.allNonNullVars ++ body.pointers.head.args.find(_.isNull)
    val res = for {
      varAssignment <- Combinators.allSeqsOfLength(localLhs.pointerVarSeq.size, bodyVars)
      // Match the pointers under the variable assignment,
      // returning the resulting var equivalence classes in case a match is possible
      reversedVarAssignment <- matchResult(localLhs, varAssignment, body)
      etype <- decompFromReverseAssignment(sid, pred, rule, localLhs.ensuredPureConstraints, reversedVarAssignment)
    } yield etype
    logger.trace(s"Found ${res.size} way(s) to match ${localLhs.sh} against rule $rule of predicate ${pred.head}")
    res
  }

  private def decompFromReverseAssignment(sid: SID, pred: Predicate, rule: RuleBody, lhsEnsuredConstraints: Set[PureAtom], reversedVarAssignment: Map[Var,Set[Var]]): Option[ContextDecomposition] = {
    // TODO: Make extension types: Split this monster of a method into smaller pieces

    logger.trace(s"Local allocation: Creating cut profile from ${pred.head}, $rule, $lhsEnsuredConstraints, $reversedVarAssignment")
    val body = rule.body
    val varsNotInAssignment = body.allNonNullVars -- reversedVarAssignment.keySet
    val placeholders = (1 to varsNotInAssignment.size) map (i => Set[Var](PlaceholderVar(i).toFreeVar))
    val placeholderMap: Map[Var,Set[Var]] = (varsNotInAssignment, placeholders).zipped.toMap
    // FIXME: Is it correct to include a mapping from null to null here?
    val combinedMap = Map[Var,Set[Var]]((NullConst, Set(NullConst))) ++ reversedVarAssignment ++ placeholderMap
    val rename = (v: Var) => combinedMap(v)

    // Compute the substitutions for the root node and the abstract leaves
    val rootSubst = Substitution(body.freeVars map rename)
    val rootNodeLabel = RuleNodeLabel(pred, rule, rootSubst)
    val childLabels = for {
      PredCall(name, args) <- body.predCalls
      pred = sid(name)
      subst = args map rename
    } yield PredicateNodeLabel(pred, Substitution(subst))

    // Compute the missing constraints
    logger.trace(s"Pure constraints of LHS: $lhsEnsuredConstraints")
    // TODO: It's kinda stupid to construct a tracker here, but that's where we currently implement the update. Should probably do some refactoring...
    val ruleConstraints: PureConstraintTracker = PureConstraintTracker(ensured = Closure.fromSH(body).asSetOfAtoms, missing = Set.empty)
    val instantiatedRuleConstraints = ruleConstraints.update(rename)
    logger.trace(s"Pure constraints of RHS $body: ${ruleConstraints.ensured}; after instantiation: $instantiatedRuleConstraints")

    val nullEqs = reversedVarAssignment.getOrElse(NullConst, Set.empty).map(NullConst =:= _)
    val otherEqs = for {
      vs <- reversedVarAssignment.values
      v <- vs
      w <- vs
      if v < w
    } yield v =:= w
    val assignmentEqs = nullEqs ++ otherEqs
    logger.trace(s"Variable assignment implies that we need the following equalities: $assignmentEqs")

    val allRuleConstraints = instantiatedRuleConstraints.ensured ++ assignmentEqs
    val missingConstraintsOfRhs = Closure.ofAtoms(allRuleConstraints).asSetOfAtoms -- lhsEnsuredConstraints
    logger.trace(s"Missing constraints to satisfy RHS: $missingConstraintsOfRhs")
    val propagatedConstraints = PureConstraintTracker(lhsEnsuredConstraints, missingConstraintsOfRhs)

    val nodeIds = NodeId.freshIds(Set.empty, childLabels.size + 1)
    val nodeLabelsMap = (nodeIds, rootNodeLabel +: childLabels).zipped.toMap
    val childMap = Map(nodeIds.head -> nodeIds.tail) ++ nodeIds.tail.zip(Stream.continually(Seq.empty))
    val ut = UnfoldingTree(nodeLabelsMap, nodeIds.head, childMap, convertToNormalform = false)
    assert(UnfoldingTree.isInNormalForm(ut), "UT for local alloc $ut is not in normal form")

    logger.trace(s"Local allocation: Unfolding tree before conversion to ET: $ut")
    val ti = ut.interface(propagatedConstraints)
    logger.trace(s"Tree interface for local allocation: $ti")

    if (!ti.allFreeRootParamsUsed) {
      // Discard sets of cuts that violate connectivity
      // TODO Can this ever be violated for BTW SIDs? Check where this occurs in the benchmarks!
      // Note: We only care about free root parameters here. If a root parameter is a placeholder, this means that the
      // corresponding predicate call is *not* directly connected to the local allocation of the rule, in which case it
      // can't be used either!
      // (Note that this can only happen for SIDs that are semantically but not syntactically in the BTW fragment.)
      // TODO Either document or drop support for this semantic extension of the BTW fragment.
      logger.trace(s"Discarding tree cuts: Not all (free) root parameters are used")
      None
    } else if (!ti.hasNamesForUsedParams) {
      logger.trace(s"Discarding cuts: Not all used parameters have names")
      None
    } else {
      Some(ti.asExtensionType)
    }
  }

  private def matchResult(localLhs: AnnotatedLocalHeap, assignment: Seq[Var], rhs: SymbolicHeap): Option[Map[Var,Set[Var]]] = {
    assert(assignment.size == localLhs.pointerVarSeq.size)
    val pairsWithPossibleDoubleCapture = localLhs.pointerVarSeq.zip(assignment)
    val (renamed,extendedF) = localLhs.sh.renameAndCreateSortedFvSequence(Renaming.fromPairs(pairsWithPossibleDoubleCapture))
    val pairs = extendedF.toPairs
    // TODO: Do we want to check pure entailment instead of syntactic equality? This depends on how we compute the var assignment. Since we're currently brute-forcing over all assignments, I don't think that's necessary. Revisit this as we implement direct matching.
    val res = renamed.pointers == rhs.pointers && ConsistencyCheck.isConsistent(renamed)
    if (res) {
      logger.trace(s"Will rename using: $pairsWithPossibleDoubleCapture")
      logger.trace(s"Renaming that avoids double capture: $pairs")
      logger.trace(s"Used $assignment to rename ${localLhs.sh} to $renamed to match against $rhs => match result: $res")
    }
    if (res) {
      // Compute reverse assignment:
      // First of all, we simply reverse the variable assignment; since the original assignment is not injective, this yields a set-valued codomain:
      val unpropagatedReverseAssignment: Map[Var,Set[Var]] = pairs.groupBy(_._2).map(pair => (pair._1, pair._2.map(_._1).toSet[Var]))
      logger.trace(s"Computed reverse assignment before propagation: $unpropagatedReverseAssignment")
      // However, this is not enough: We need to take equalities into account: If two variables are known to be equal,
      // both of these variables must be mapped to the same values, the union of their values before propagation:
      val reverseAssignment = propagateEqualitiesIntoReverseAssignment(localLhs.sh, rhs, unpropagatedReverseAssignment)
      logger.trace(s"Reverse assignment after propagation: $reverseAssignment")
      Some(reverseAssignment)
    }
    else None
  }

  private def propagateEqualitiesIntoReverseAssignment(lhsLocal: SymbolicHeap, rhs: SymbolicHeap, unpropagatedReverseAssignment: Map[Var, Set[Var]]): Map[Var, Set[Var]] = {
    val rhsClosure = Closure.fromSH(rhs)
    logger.trace(s"Reverse atom propagation: Closure of rhs: $rhsClosure")

    val lhsClosure = Closure.ofAtoms(lhsLocal.pure.filter(_.isEquality))
    logger.trace(s"Reverse atom propagation: Equalities of the LHS: $lhsClosure")

    val reverseAssignmentPairs = for {
      // For each variable of the RHS...
      v <- NullConst +: (rhs.freeVars ++ rhs.boundVars)
      combined = for {
        // ...there are three types of equalities that must be propagated throughout this assignment:
        // 1.) Equalities that are explicit in the RHS rule body.
        // If x = y in the RHS, this means that x and y must be mapped to the same values in the reverse assignment
        // These must be hold in every matching of the LHS against the RHS
        // They can be accessed via a closure of the RHS
        srcVar <- rhsClosure.getEquivalenceClass(v)
        // 2.) Equalities explicit in the reverse assignment
        // We map all rhs variables that are equal to v to all the targets of their respective reverse assignment
        // (The reverse assignment maps RHS variables onto LHS variables)
        trgVar <- unpropagatedReverseAssignment.getOrElse(srcVar, Set.empty)
        // 3.) Equalities that are explicit in the LHS heap;
        // These must of course be respected in the matching and be reflected in the variable assignment
        eqVar <- lhsClosure.getEquivalenceClass(trgVar)
      } yield eqVar
      if combined.nonEmpty
    } yield (v, combined)
    reverseAssignmentPairs.toMap
  }
}
