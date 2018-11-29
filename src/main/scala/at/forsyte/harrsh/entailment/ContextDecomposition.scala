package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PredCall, Predicate, PureAtom, SID}
import at.forsyte.harrsh.util.ToLatex

/**
  * A single context decomposition
  * @param parts Contexts that make up the decomposition
  */
case class ContextDecomposition(parts: Set[EntailmentContext]) extends HarrshLogging {

  assert(parts forall EntailmentContext.isInNormalForm,
    s"Trying to construct extension type from non-normalized parts ${parts.mkString(", ")}")
  assert(parts forall (_.hasConsistentPureConstraints),
    s"Contradictory pure constraints in tree interface ${parts.find(!_.hasConsistentPureConstraints).get}")

  lazy val missingPureConstraints: Set[PureAtom] = parts.flatMap(_.pureConstraints.missing)

  lazy val rootParamSubsts: Seq[Set[Var]] = parts.toSeq.flatMap(_.rootParamSubsts)

  def hasNamesForAllRootParams: Boolean = parts.forall(_.hasNamesForRootParams)

  def isSingletonDecomposition: Boolean = parts.size == 1

  def containsMultipleContextsWithRoots(pred: Predicate): Boolean = {
    parts.count(_.root.pred == pred) > 1
  }

  def isViable(sid: SID): Boolean = {
    // We don't do a full viability check (yet)
    // This overapproximation of viability discards those decompositions that contain two contexts rooted in a predicate that can
    // occur at most once in any given sid-unfolding tree.
    !sid.predsThatOccurAtMostOnceInUnfolding.exists(containsMultipleContextsWithRoots)
  }

  def isFinal(calls: PredCalls): Boolean = {
    val res = if (parts.size != calls.size) {
      false
    } else {
      val roots = parts map (_.root)
      Stream(
        parts forall (_.isConcrete),
        parts forall (_.pureConstraints.missing.isEmpty),
        calls.implies(roots)
      ).forall(b => b)
    }
    logger.trace(s"Checking whether $this is final w.r.t. $calls => $res")
    res
  }

  lazy val nonNullNonPlaceholderVars: Set[Var] = parts.flatMap(_.nonNullNonPlaceholderVars)

  lazy val boundVars: Set[BoundVar] = parts.flatMap(_.boundVars)

  lazy val placeholders: Set[PlaceholderVar] = parts.flatMap(_.placeholders)

  /**
    * Check whether dropping the given var would put one or missing constraints out of scope---thus meaning the entailment can never become true.
    */
  private def tryingToDropVarsWithMissingConstraints(varsToDrop: Set[Var]): Boolean = {
    (missingPureConstraints.flatMap(_.getNonNullVars) intersect varsToDrop).nonEmpty
  }

  def forget(varsToForget: Set[Var]): Option[ContextDecomposition] = {
    if (varsToForget.isEmpty) {
      Some(this)
    } else {
      logger.debug(s"Will remove bound variables ${varsToForget.mkString(",")} from decomposition")
      if (tryingToDropVarsWithMissingConstraints(varsToForget)) {
        // We're forgetting variables for which there are still missing constraints
        // After dropping, it will no longer be possible to supply the constraints
        // We hence discard the extension type
        logger.debug(s"Discarding decomposition: Missing pure constraints $missingPureConstraints contain at least one discarded variable from ${varsToForget.mkString(", ")}")
        None
      } else {
        // From the pure constraints, we must remove the variables completely, because after forgetting a variable,
        // the (ensured) constraints become meaningless for the context...
        val partsAfterDroppingPureConstraints = parts map (_.dropVarsFromPureConstraints(varsToForget.toSet))

        // ...whereas in the interface nodes/usage info, we replace the bound vars by placeholders
        // TODO: More efficient variable dropping for decomps
        // Rename the vars to fresh placeholder vars
        val maxPlaceholder = PlaceholderVar.maxIndex(placeholders)
        val newPlaceholders = (1 to varsToForget.size) map (i => PlaceholderVar(maxPlaceholder + i))
        val pairs: Seq[(Var, Var)] = varsToForget.toSeq.zip(newPlaceholders.map(_.toFreeVar))
        val replaceByPlacheolders: SubstitutionUpdate = SubstitutionUpdate.fromPairs(pairs)


        // Note: Must to normalization to get rid of gaps in results and strange order by doing normalization
        val partsAfterDropping = partsAfterDroppingPureConstraints map (_.updateSubst(replaceByPlacheolders, convertToNormalform = true))

        Some(ContextDecomposition(partsAfterDropping))
      }
    }
  }

  def updateSubst(f: SubstitutionUpdate): ContextDecomposition = {
    ContextDecomposition(parts map (_.updateSubst(f, convertToNormalform = false)))
  }

  def compositionOptions(other: ContextDecomposition): Seq[ContextDecomposition] = {
    for {
      composed <- EntailmentContextComposition.compositionOptions(parts.toSeq ++ other.parts)
      if composed.forall(_.hasConsistentPureConstraints)
    } yield ContextDecomposition(composed)
  }

  def isInconsistent: Boolean = {
    allocsNull || doubleAlloc
  }

  private def allocsNull: Boolean = {
    rootParamSubsts exists Var.containsNull
  }

  private def doubleAlloc: Boolean = {
    val doublyAlloced = for {
      (k, vs) <- rootParamSubsts.groupBy(vs => vs).toStream
      if vs.size > 1
    } yield k
    doublyAlloced.nonEmpty
  }

}

object ContextDecomposition {

  implicit val decompToLatex: ToLatex[ContextDecomposition] = EntailmentResultToLatex.decompToLatex

  def apply(parts: Seq[EntailmentContext]): ContextDecomposition = ContextDecomposition(parts.toSet)



}
