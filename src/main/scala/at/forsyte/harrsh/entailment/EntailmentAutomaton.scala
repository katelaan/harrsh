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

  // TODO: Support for null param in rhs?

  // The entailment checker only works for rooted SIDs that satisfy progress
  assert(sid.isRooted)
  assert(sid.satisfiesProgress)

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

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug(s"Computing target for $lab from $src")
    val localETs = EntailmentAutomaton.shToExtensionTypes(lab, sid)
    if (localETs.isEmpty) {
      logger.debug(s"No extension types for local allocation $lab => Return inconsistent state")
      Set(EntailmentAutomaton.State(Set.empty, lab.freeVars))
    } else {
      logger.debug(s"Extension types for local allocation of $lab from $src:\n${localETs.mkString("\n")}")
      val instantiatedETs = (src, lab.predCalls).zipped.map(instantiateETsForCall)
      for {
        (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
      } {
        logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
      }
      val allETs: Seq[Set[ExtensionType]] = localETs +: instantiatedETs

      // Compose the extension types
      val targetETs = for {
        ets <- Combinators.choices(allETs map (_.toSeq)).toSet[Seq[ExtensionType]]
        combined <- combineETs(ets, lab)
      } yield combined
      logger.debug(s"Target state:\n${if (targetETs.nonEmpty) targetETs.mkString("\n") else "empty (sink state)"}")

      Set(EntailmentAutomaton.State(targetETs, lab.freeVars))
    }
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

  def shToExtensionTypes(lhs: SymbolicHeap, sid: SID) : Set[ExtensionType] = {
    // TODO: Deal with multi-pointer and zero-pointer SHs for the left-hand side? This is particularly interesting for top-level formulas. (We could do this either through rewriting (converting the LHS into a rooted SID) or through explicit support (implementing partial model checking).)
    assert(lhs.pointers.size == 1)

    logger.debug(s"Creating (${sid.description})-extension types from $lhs")

    for {
      pred <- sid.preds.toSet[Predicate]
      rule <- pred.rules
      body = rule.body
      lhsLocal = lhs.withoutCalls
      // Rename the variables that occur in the points-to assertion. (Only those, because we match only the pointers)
      lhsLocalVars = lhsLocal.pointers.head.getNonNullVars.toSeq
      lhsEnsuredDiseqs = Closure.fromSH(lhsLocal).asSetOfAtoms.filter(!_.isEquality)
      // Go over all ways to map a variable of the lhs to a variable of the rhs
      // FIXME: Don't brute force over assignments, but pick the suitable one(s) directly
      varAssignment <- Combinators.allSeqsOfLength(lhsLocalVars.size, body.allVars)
      // Match the pointers under the variable assignment,
      // returning the resulting var equivalence classes in case a match is possible
      reversedVarAssignment <- matchResult(lhsLocal, lhsLocalVars, varAssignment, body)
    } yield mkExtensionType(sid, pred, rule, lhsEnsuredDiseqs, reversedVarAssignment)
  }

  private def mkExtensionType(sid: SID, pred: Predicate, rule: RuleBody, lhsEnsuredDiseqs: Set[PureAtom], varAssignment: Map[Var,Set[Var]]): ExtensionType = {
    logger.debug(s"Creating extension type from ${pred.head}, $rule, $lhsEnsuredDiseqs, $varAssignment")
    val body = rule.body
    val varsNotInAssignment = body.allVars -- varAssignment.keySet
    val placeholders = (1 to varsNotInAssignment.size) map (i => Set[Var](PlaceholderVar(i).toFreeVar))
    val placeholderMap: Map[Var,Set[Var]] = (varsNotInAssignment, placeholders).zipped.toMap
    val combinedMap = varAssignment ++ placeholderMap
    val rename = (v: Var) => combinedMap(v)

    // Compute the substitutions for the root node and the abstract leaves
    val rootSubst = Substitution(body.freeVars map rename)
    val rootNodeLabel = RuleNodeLabel(pred, rule, rootSubst)
    val childLabels = for {
      PredCall(name, args) <- body.predCalls
      pred = sid(name)
      subst = args map rename
    } yield AbstractLeafNodeLabel(pred, Substitution(subst))

    // Compute the missing disequalities
    val ruleDiseqs: DisequalityTracker = DisequalityTracker(ensured = Closure.fromSH(body).asSetOfAtoms.filter(!_.isEquality), missing = Set.empty)
    val instantiatedRuleDiseqs = ruleDiseqs.update(rename)
    val missingDisEqsOfRhs = instantiatedRuleDiseqs.ensured -- lhsEnsuredDiseqs
    val propagatedDiseqs = DisequalityTracker(lhsEnsuredDiseqs, missingDisEqsOfRhs)

    val nodeIds = NodeId.freshIds(Set.empty, childLabels.size + 1)
    val nodeLabelsMap = (nodeIds, rootNodeLabel +: childLabels).zipped.toMap
    val childMap = Map(nodeIds.head -> nodeIds.tail) ++ nodeIds.tail.zip(Stream.continually(Seq.empty))
    val ut = UnfoldingTree(nodeLabelsMap, nodeIds.head, childMap, convertToNormalform = false)
    assert(UnfoldingTree.isInNormalForm(ut), "UT for local alloc $ut is not in normal form")

    logger.debug(s"Unfolding tree before conversion to ET: $ut")
    ut.interface(propagatedDiseqs).asExtensionType
  }

  private def matchResult(lhsLocal: SymbolicHeap, lhsLocalVars: Seq[Var], assignment: Seq[Var], rhs: SymbolicHeap): Option[Map[Var,Set[Var]]] = {
    assert(assignment.size == lhsLocalVars.size)
    val pairs = lhsLocalVars.zip(assignment)
    val renamed = lhsLocal.rename(Renaming.fromPairs(pairs))
    // TODO: Do we want to check pure entailment instead of syntactic equality? This depends on how we compute the var assignment. Since we're currently brute-forcing over all assignments, I don't think that's necessary. Revisit this as we implement direct matching.
    val res = renamed.pointers == rhs.pointers && ConsistencyCheck.isConsistent(renamed)
    if (res) {
      logger.debug(s"Used $assignment to rename $lhsLocal to $renamed to match against $rhs => match result: $res")
    }
    if (res) {
      // Compute reverse assignment
      val unpropagatedReverseAssignment: Map[Var,Set[Var]] = pairs.groupBy(_._2).map(pair => (pair._1, pair._2.map(_._1).toSet[Var]))
      logger.debug(s"Computed reverse assignment before propagation: $unpropagatedReverseAssignment")

      val shAtoms = lhsLocal.pure :+ (lhsLocal.pointers.head.from =/= NullConst)
      def toEqs(vs: Set[Var]): Set[PureAtom] = if (vs.size <= 1) Set.empty else {
        val head = vs.head
        val tail = vs - head
        (tail map (head =:= _)) ++ toEqs(tail)
      }
      val revAssAtoms = unpropagatedReverseAssignment.values.flatMap(toEqs)
      val closure = Closure.ofAtoms(shAtoms ++ revAssAtoms)
      val reverseAssignmentPairs = for {
        fv <- rhs.allVars //freeVars
        explicit = unpropagatedReverseAssignment.getOrElse(fv, Set.empty)
        implied = closure.getEquivalenceClass(fv, defaultToSingletonClass = false)
        combined = explicit ++ implied
        if combined.nonEmpty
      } yield (fv.asInstanceOf[Var], combined)
      val reverseAssignment = reverseAssignmentPairs.toMap

      logger.debug(s"Reverse assignment after propagation: $reverseAssignment")

      Some(reverseAssignment)
    }
    else None
  }

  private def rename(sh: SymbolicHeap, perm: Seq[FreeVar]): SymbolicHeap = {
    val renamingPairs = sh.freeVars.zip(perm)
    val renaming = Renaming.fromPairs(renamingPairs)
    sh.rename(renaming)
  }
}
