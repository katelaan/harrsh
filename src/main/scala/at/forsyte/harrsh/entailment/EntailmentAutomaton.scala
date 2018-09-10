package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{Closure, ConsistencyCheck, PureEntailment}
import at.forsyte.harrsh.seplog.{FreeVar, NullConst, Renaming, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

/**
  * An entailment automaton for entailment instances phi |=_sid rhs, where sid and rhs are fixed.
  * @param sid The SID definining the meaning of the rhs.
  * @param rhs The predicate call on the right-hand side of the entailment.
  */
class EntailmentAutomaton(sid: SID, rhs: PredCall) extends HeapAutomaton with InconsistentState {

  // The entailment checker only works for rooted SIDs that satisfy progress
  // TODO: Check rootedness and progress for the part of the SID that does not correspond to the top-level predicate?
  //assert(sid.isRooted)
  //assert(sid.satisfiesProgress)

  assert(rhs.args forall (_.isFree))

  override val description: String = s"EntailmentAutomaton($rhs)"

  override type State = EntailmentAutomaton.State

  override def isFinal(s: State): Boolean = {
    // A state represents all the possible ways to parse an RSH as a SID unfolding tree.
    // As long as one of those ways is a valid unfolding tree, we accept.
    val res = s.ets.exists(_.isFinal(rhs))
    logger.trace(s"Checked wheter $s is final => $res")
    res
  }

  /**
    * An inconsistent state representing all "sink" states
    */
  override def inconsistentState(fvs: Seq[FreeVar]): State = EntailmentAutomaton.State(Set.empty, fvs)

  sealed trait LocalETs {
    def areDefined: Boolean = this match {
      case NoLocalETs => true
      case ETsOfLocalAtoms(ets) => ets.nonEmpty
    }

    def ++(other: Seq[Set[ExtensionType]]) : Seq[Set[ExtensionType]] = this match {
      case NoLocalETs => other
      case ETsOfLocalAtoms(ets) => ets +: other
    }
  }
  case object NoLocalETs extends LocalETs
  case class ETsOfLocalAtoms(ets: Set[ExtensionType]) extends LocalETs

  private def localETs(lab: SymbolicHeap) : LocalETs = {
    if (lab.pointers.isEmpty) {
      if (lab.pure.nonEmpty) {
        throw new IllegalArgumentException(s"Can't process symbolic heap without allocation but with pure constraints: $lab")
      } else {
        logger.debug(s"No pointers/pure atoms in rule => don't compute local ETs")
        NoLocalETs
      }
    } else {
      val res = EntailmentAutomaton.localAllocToExtensionTypes(lab, sid)
      logger.debug(s"Extension types for local allocation of $lab:\n${res.mkString("\n")}")
      ETsOfLocalAtoms(res)
    }
  }

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug(s"Computing target for $lab from source states:\n${src.mkString("\n")}")
    val local = localETs(lab)
    if (local.areDefined) {
      val instantiatedETs = instantiatedSourceStates(src, lab)
      val allETs: Seq[Set[ExtensionType]] = local ++ instantiatedETs
      val targetETs = allPossibleETCompositions(allETs, lab)
      logger.debug(s"Target state:\n${if (targetETs.nonEmpty) targetETs.mkString("\n") else "empty (sink state)"}")
      Set(EntailmentAutomaton.State(targetETs, lab.freeVars))
    } else {
      logger.debug(s"No extension types for local allocation $lab => Return inconsistent state")
      Set(EntailmentAutomaton.State(Set.empty, lab.freeVars))
    }
  }

  private def instantiatedSourceStates(src: Seq[State], lab: SymbolicHeap): Seq[Set[ExtensionType]] = {
    val instantiatedETs = (src, lab.predCalls).zipped.map(instantiateETsForCall)
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    instantiatedETs
  }

  private def allPossibleETCompositions(etsByState: Seq[Set[ExtensionType]], lab: SymbolicHeap): Set[ExtensionType] = {
    for {
      ets <- Combinators.choices(etsByState map (_.toSeq)).toSet[Seq[ExtensionType]]
      combined <- combineETs(ets, lab)
    } yield combined
  }

  private def composeAll(ets: Seq[ExtensionType]): Option[ExtensionType] = {
    if (ets.size <= 1) ets.headOption else {
      for {
        combinedHead <- ets.head compose ets.tail.head
        allComposed <- composeAll(combinedHead +: ets.drop(2))
      } yield allComposed
    }
  }

  private def combineETs(ets: Seq[ExtensionType], lab: SymbolicHeap): Option[ExtensionType] = {
    logger.debug(s"Trying to combine:\n${ets.map(_.parts.mkString("ET(", "\n  ", ")")).mkString("\n")}\nw.r.t. symbolic heap $lab...")
    val res = for {
      combined <- composeAll(ets)
      _ = logger.debug(s"Resulting parts of the ET:\n${combined.parts.mkString("\n")}")
      _ = logger.debug(s"Bound vars in result: ${combined.boundVars} (in the symbolic heap: ${lab.boundVars.mkString(",")})")
      // Drop the bound vars from the extension types, since they are no longer needed after the composition
      _ = assert(combined.boundVars.diff(lab.boundVars).isEmpty,
        s"Extension type $combined contains bound vars not in symbolic heap $lab")
      restrictedToFreeVars <- combined.dropVars(lab.boundVars.toSeq)
      _ = logger.debug(s"After restriction to free variables:\n${restrictedToFreeVars.parts.mkString("\n")}")
      _ = assert(restrictedToFreeVars.boundVars.isEmpty, s"Bound vars remain after restriction to free vars: $restrictedToFreeVars")
      // Filter out extension types that don't have free vars in all root positions
      // FIXME: Also filter out types that lack names for back pointers
      // FIXME: What to check for the top-level predicates that don't have a root parameter annotation?
      if restrictedToFreeVars.hasNamesForRootParams
      _ = logger.debug(s"Extension type is consistent, will become part of target state.")
    } yield restrictedToFreeVars

    if (res.isEmpty) {
      logger.debug("Could not combine ETs.")
    }

    res
  }

  private def instantiateETsForCall(state: State, call: PredCall): Set[ExtensionType] = {
    val EntailmentAutomaton.State(ets, params) = state
    val callUpdate: SubstitutionUpdate = v => {
      // If v is the i-th free variable of the predicate, replace it with the i-th argument of the call;
      // otherwise, return the variable as is
      val fvIx = params.indexOf(v)
      if (fvIx >= 0) Set(call.args(fvIx)) else Set(v)
    }
    ets.map(_.updateSubst(callUpdate))
  }

}

object EntailmentAutomaton extends HarrshLogging {

  case class State(ets: Set[ExtensionType], orderedParams: Seq[FreeVar]) {

    assert(ets forall (_.boundVars.isEmpty),
      s"Trying to construct state from extension types that still contain bound vars: $ets")

    private val freeVarsInEts = ets.flatMap(_.nonPlaceholderFreeVars)
    //if (ets.nonEmpty && freeVarsInEts != orderedParams.toSet) {
    if (ets.nonEmpty && !(freeVarsInEts subsetOf orderedParams.toSet)) {
      throw new IllegalArgumentException(s"ETs contain FVs $freeVarsInEts, but constructing state for $orderedParams")
    }

  }

  private case class AnnotatedLocalHeap(sh: SymbolicHeap, pointerVarSeq: Seq[Var], ensuredPureConstraints: Set[PureAtom])

  private def annotatedLocalHeap(lhs: SymbolicHeap): AnnotatedLocalHeap = {
    assert(lhs.pointers.size == 1)
    val localLhs = lhs.withoutCalls
    // Rename the variables that occur in the points-to assertion. (Only those, because we match only the pointers)
    val localPtr = localLhs.pointers.head
    val pointerVarSeq = localPtr.getNonNullVars.toSeq ++ localPtr.args.find(_ == NullConst)
    val ensuredPureConstraints = Closure.fromSH(localLhs).asSetOfAtoms
    AnnotatedLocalHeap(localLhs, pointerVarSeq, ensuredPureConstraints)
  }

  def localAllocToExtensionTypes(lhs: SymbolicHeap, sid: SID) : Set[ExtensionType] = {
    logger.debug(s"Local allocation: Creating (${sid.description})-extension types from $lhs")
    val localLhs = annotatedLocalHeap(lhs)

    for {
      pred <- sid.preds.toSet[Predicate]
      rule <- pred.rules
      etype <- mkExtensionType(sid, pred, rule, localLhs)
    } yield etype
  }

  private def mkExtensionType(sid: SID, pred: Predicate, rule: RuleBody, localLhs: AnnotatedLocalHeap): Set[ExtensionType] = {
    // Go over all ways to map a variable of the lhs to a variable of the rhs
    // FIXME: Don't brute force over assignments, but pick the suitable one(s) directly
    val body = rule.body
    val bodyVars = body.allNonNullVars ++ body.pointers.head.args.find(_.isNull)
    val res = for {
      varAssignment <- Combinators.allSeqsOfLength(localLhs.pointerVarSeq.size, bodyVars)
      // Match the pointers under the variable assignment,
      // returning the resulting var equivalence classes in case a match is possible
      reversedVarAssignment <- matchResult(localLhs, varAssignment, body)
      etype <- extensionTypeFromReverseAssignment(sid, pred, rule, localLhs.ensuredPureConstraints, reversedVarAssignment)
    } yield etype
    logger.debug(s"Found ${res.size} way(s) to match ${localLhs.sh} against rule $rule of predicate ${pred.head}")
    res
  }

  private def extensionTypeFromReverseAssignment(sid: SID, pred: Predicate, rule: RuleBody, lhsEnsuredConstraints: Set[PureAtom], reversedVarAssignment: Map[Var,Set[Var]]): Option[ExtensionType] = {
    // TODO: Make extension types: Split this monster of a method into smaller pieces

    logger.debug(s"Local allocation: Creating extension type from ${pred.head}, $rule, $lhsEnsuredConstraints, $reversedVarAssignment")
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
    } yield AbstractLeafNodeLabel(pred, Substitution(subst))

    // Compute the missing constraints
    logger.debug(s"Pure constraints of LHS: $lhsEnsuredConstraints")
    // TODO: It's kinda stupid to construct a tracker here, but that's where we currently implement the update. Should probably do some refactoring...
    val ruleConstraints: PureConstraintTracker = PureConstraintTracker(ensured = Closure.fromSH(body).asSetOfAtoms, missing = Set.empty)
    val instantiatedRuleConstraints = ruleConstraints.update(rename)
    logger.debug(s"Pure constraints of RHS $body: ${ruleConstraints.ensured}; after instantiation: $instantiatedRuleConstraints")

    val nullEqs = reversedVarAssignment.getOrElse(NullConst, Set.empty).map(NullConst =:= _)
    val otherEqs = for {
      vs <- reversedVarAssignment.values
      v <- vs
      w <- vs
      if v < w
    } yield v =:= w
    val assignmentEqs = nullEqs ++ otherEqs
    logger.debug(s"Variable assignment implies that we need the following equalities: $assignmentEqs")

    val allRuleConstraints = instantiatedRuleConstraints.ensured ++ assignmentEqs
    val missingConstraintsOfRhs = Closure.ofAtoms(allRuleConstraints).asSetOfAtoms -- lhsEnsuredConstraints
    logger.debug(s"Missing constraints to satisfy RHS: $missingConstraintsOfRhs")
    val propagatedConstraints = PureConstraintTracker(lhsEnsuredConstraints, missingConstraintsOfRhs)

    val nodeIds = NodeId.freshIds(Set.empty, childLabels.size + 1)
    val nodeLabelsMap = (nodeIds, rootNodeLabel +: childLabels).zipped.toMap
    val childMap = Map(nodeIds.head -> nodeIds.tail) ++ nodeIds.tail.zip(Stream.continually(Seq.empty))
    val ut = UnfoldingTree(nodeLabelsMap, nodeIds.head, childMap, convertToNormalform = false)
    assert(UnfoldingTree.isInNormalForm(ut), "UT for local alloc $ut is not in normal form")

    logger.debug(s"Local allocation: Unfolding tree before conversion to ET: $ut")
    val ti = ut.interface(propagatedConstraints)
    logger.debug(s"Tree interface for local allocation: $ti")

    if (!ti.allRootParamsUsed) {
      logger.debug(s"Discarding extension type: Not all root parameters are used")
      None
    } else if (!ti.hasNamesForUsedParams) {
      logger.debug(s"Discarding extension type: Not all used parameters have names")
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
      logger.debug(s"Will rename using: $pairsWithPossibleDoubleCapture")
      logger.debug(s"Renaming that avoids double capture: $pairs")
      logger.debug(s"Used $assignment to rename ${localLhs.sh} to $renamed to match against $rhs => match result: $res")
    }
    if (res) {
      // Compute reverse assignment:
      // First of all, we simply reverse the variable assignment; since the original assignment is not injective, this yields a set-valued codomain:
      val unpropagatedReverseAssignment: Map[Var,Set[Var]] = pairs.groupBy(_._2).map(pair => (pair._1, pair._2.map(_._1).toSet[Var]))
      logger.debug(s"Computed reverse assignment before propagation: $unpropagatedReverseAssignment")
      // However, this is not enough: We need to take equalities into account: If two variables are known to be equal,
      // both of these variables must be mapped to the same values, the union of their values before propagation:
      val reverseAssignment = propagateEqualitiesIntoReverseAssignment(localLhs.sh, rhs, unpropagatedReverseAssignment)
      logger.debug(s"Reverse assignment after propagation: $reverseAssignment")
      Some(reverseAssignment)
    }
    else None
  }

  private def propagateEqualitiesIntoReverseAssignment(lhsLocal: SymbolicHeap, rhs: SymbolicHeap, unpropagatedReverseAssignment: Map[Var, Set[Var]]): Map[Var, Set[Var]] = {
    val rhsClosure = Closure.fromSH(rhs)
    logger.debug(s"Reverse atom propagation: Closure of rhs: $rhsClosure")

    val lhsClosure = Closure.ofAtoms(lhsLocal.pure.filter(_.isEquality))
    logger.debug(s"Reverse atom propagation: Equalities of the LHS: $lhsClosure")

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

  private def rename(sh: SymbolicHeap, perm: Seq[FreeVar]): SymbolicHeap = {
    val renamingPairs = sh.freeVars.zip(perm)
    val renaming = Renaming.fromPairs(renamingPairs)
    sh.rename(renaming, overrideFreeVars = None)
  }
}
