package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{Closure, PureEntailment}
import at.forsyte.harrsh.seplog.{FreeVar, Renaming, Var}
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
    logger.debug(s"Checked wheter $s is final => $res")
    res
  }

  /**
    * An inconsistent state representing all "sink" states
    */
  override def inconsistentState(fvs: Seq[FreeVar]): State = EntailmentAutomaton.State(Set.empty, fvs)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug(s"Computing target for $lab from $src")
    val localETs = EntailmentAutomaton.shToExtensionTypes(lab, sid)
    logger.debug(s"Extension types for $lab:\n${localETs.mkString("\n")}")
    val instantiatedETs = (src, lab.predCalls).zipped.map(instantiateETsForCall)
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    val allETs: Seq[Set[ExtensionType]] = localETs +: instantiatedETs
    // Compose the extension types
    val reachable = for {
      ets <- Combinators.choices(allETs map (_.toSeq))
    } yield ets.reduceLeft(_ compose _)
    logger.debug(s"Reachable extension types (including bound vars):\n${reachable.mkString("\n")}")

    // Drop the bound vars from the extension types, since they are no longer needed after the composition
    val restrictedToFreeVars = reachable map (_.dropVars(lab.boundVars.toSeq))
    logger.debug(s"Target state:\n${restrictedToFreeVars.mkString("\n")}")

    // Filter out extension types that don't have free vars in all root positions
    // FIXME: Also filter out types that lack names for back pointers
    val consistent = restrictedToFreeVars filter (_.hasNamesForRootParams)

    Set(EntailmentAutomaton.State(consistent.toSet, lab.freeVars))
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

    private val freeVarsInEts = ets.flatMap(_.nonPlaceholderFreeVars)
    if (freeVarsInEts != orderedParams.toSet) {
      throw new IllegalArgumentException(s"ETs contain FVs $freeVarsInEts, but constructing state for $orderedParams")
    }

  }

  def shToExtensionTypes(sh: SymbolicHeap, sid: SID) : Set[ExtensionType] = {
    // TODO: Deal with multi-pointer and zero-pointer SHs for the left-hand side? This is particularly important for top-level formulas. (We could do this either through rewriting (converting the LHS into a rooted SID) or through explicit support (implementing partial model checking).)
    assert(sh.pointers.size == 1)

    logger.debug(s"Creating (${sid.description})-extension types from $sh")

    for {
      pred <- sid.preds.toSet[Predicate]
      // Only consider rules with the same number of free variables
      if pred.arity == sh.numFV
      rule <- pred.rules
      // Instantiate the rule body with the actual free vars of the left-hand side
      ruleInstance = PredCall(pred.head, sh.freeVars).toSymbolicHeap.replaceCalls(Seq(rule.body))
      freeVarPermutation <- Combinators.permutations(sh.freeVars)
      shInstance = rename(sh, freeVarPermutation)
      varAssignment <- matchHeaps(shInstance, ruleInstance)
      instantiatedAssignment = reversePermutation(varAssignment, sh.freeVars, freeVarPermutation)
    } yield mkExtensionType(sid, pred, rule, instantiatedAssignment)
  }

  private def rename(sh: SymbolicHeap, perm: Seq[FreeVar]): SymbolicHeap = {
    val renamingPairs = sh.freeVars.zip(perm)
    val renaming = Renaming.fromPairs(renamingPairs)
    sh.rename(renaming)
  }


  def reversePermutation(varAssignment: Map[Var, Var], originalFVs: Seq[FreeVar], permutedFVs: Seq[FreeVar]): Map[Var,Var] = {
    val map: Map[Var, Var] = (permutedFVs, originalFVs).zipped.toMap
    varAssignment.map {
      case (k,v) => (k,map.getOrElse(v,v))
    }
  }

  private def compatiblePointers(leftPtr: PointsTo, leftEqs: Closure, rightPtr: PointsTo): Boolean = {
    // Pointers are compatible if all arguments coincide (modulo the equalities defined in the left heap)
    //        val matchResults = for {
    //          (xl, xr) <- (leftPtr.args, rightPtr.args).zipped
    //        } yield leftEqs.getEquivalenceClass(xl).contains(xr)
    //        matchResults.forall(b => b)
    // TODO: Only the LHS has to coincide. I.e., the following test should be sufficient?
    leftEqs.getEquivalenceClass(leftPtr.from).contains(rightPtr.from)
  }

  private def matchHeaps(leftHeap: SymbolicHeap, rightRuleInstance: SymbolicHeap): Set[Map[Var, Var]] = {
    logger.debug(s"Try matching $leftHeap against $rightRuleInstance")

    // FIXME: Generate all possible mappings (involving all ways that vars are allowed to alias without contradicting the RHS closure) rather than just one default substitution
    // TODO: This equality- and unification-based reasoning should have significant overlap with existing code. (Model checking, tracking etc.) Should clean all that up and provide a set of common core routines!
    // TODO: Explicitly include the != null constraints for allocation?
    val leftEqs = Closure.ofAtoms(leftHeap.pure)
    val rightEqs = Closure.ofAtoms(rightRuleInstance.pure)

    // TODO: After debugging, this should be moved into the if for possible short-circuiting
    val leftEqsEntailRightEqs = PureEntailment.check(leftEqs, rightEqs)
    val pointersCompatible = compatiblePointers(leftHeap.pointers.head, leftEqs, rightRuleInstance.pointers.head)
    if (leftEqsEntailRightEqs && pointersCompatible) {
      val map = (rightRuleInstance.pointers.head.args, leftHeap.pointers.head.args).zipped.toMap
      Set(map)
    } else {
      logger.debug(s"Couldn't match $leftHeap against $rightRuleInstance. Pure entailment holds: $leftEqsEntailRightEqs; Pointers compatible: $pointersCompatible")
      Set.empty
    }
  }


  private def mkExtensionType(sid: SID, pred: Predicate, rule: RuleBody, varAssignment: Map[Var,Var]): ExtensionType = {
    logger.debug(s"Creating extension type from ${pred.head}, $rule, $varAssignment")

    val body = rule.body
    val varsNotInAlloc = (body.allVars -- varAssignment.keySet).toSeq.sorted
    val placeholders = (1 to varsNotInAlloc.size) map (PlaceholderVar(_).toFreeVar)
    val placeholderMap = (varsNotInAlloc, placeholders).zipped.toMap
    val combinedMap = varAssignment ++ placeholderMap
    val rename = (v: Var) => Set(combinedMap(v))

    // Compute the substitutions for the root node and the abstract leaves
    val rootSubst = Substitution(body.freeVars map rename)
    val rootNodeLabel = RuleNodeLabel(pred, rule, rootSubst)
    val childLabels = for {
      PredCall(name, args) <- body.predCalls
      pred = sid(name)
      subst = args map rename
    } yield AbstractLeafNodeLabel(pred, Substitution(subst))

    val nodeIds = NodeId.freshIds(Set.empty, childLabels.size + 1)
    val nodeLabelsMap = (nodeIds, rootNodeLabel +: childLabels).zipped.toMap
    val childMap = Map(nodeIds.head -> nodeIds.tail) ++ nodeIds.tail.zip(Stream.continually(Seq.empty))
    val ut = UnfoldingTree(nodeLabelsMap, nodeIds.head, childMap)

    logger.debug(s"Unfolding tree before conversion to ET: $ut")
    ut.interface.asExtensionType
  }

}
