package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PredCall, Predicate, PureAtom, SID}
import at.forsyte.harrsh.util.ToLatex

/**
  * A single abstracted forest, retaining only the tree interfaces rather than the full trees.
  * @param parts Abstracted trees
  */
case class ExtensionType(parts: Set[TreeInterface]) extends HarrshLogging {

  assert(parts forall TreeInterface.isInNormalForm,
    s"Trying to construct extension type from non-normalized parts ${parts.mkString(", ")}")
  assert(parts forall (_.hasConsistentPureConstraints),
    s"Contradictory pure constraints in tree interface ${parts.find(!_.hasConsistentPureConstraints).get}")

  override def toString: String = parts.mkString("ET(", ",\n   ", ")")

  lazy val missingPureConstraints: Set[PureAtom] = parts.flatMap(_.pureConstraints.missing)

  lazy val rootParamSubsts: Seq[Set[Var]] = parts.toSeq.flatMap(_.rootParamSubsts)

  def hasNamesForAllRootParams: Boolean = parts.forall(_.hasNamesForRootParams)

  def representsSingleTree: Boolean = parts.size == 1

  def containsMultipleRootsOf(pred: Predicate): Boolean = {
    parts.count(_.root.pred == pred) > 1
  }

  def isViable(sid: SID): Boolean = {
    // We don't do a full viability check (yet)
    // This overapproximation of viability discards those forests that contain two trees rooted in a predicate that can
    // occur at most once in an unfolding tree.
    !sid.predsThatOccurAtMostOnceInUnfolding.exists(containsMultipleRootsOf)
  }

  def isFinal(call: PredCall): Boolean = {
    val res = if (!representsSingleTree) {
      // Only single (abstracted) trees can be final
      false
    } else {
      parts.head.isFinalFor(call)
    }
    logger.trace(s"Checking whether $this is final w.r.t. $call => $res")
    res
  }

  lazy val nonPlaceholderFreeVars: Set[FreeVar] = parts.flatMap(_.nonPlaceholderFreeVars)

  lazy val boundVars: Set[BoundVar] = parts.flatMap(_.boundVars)

  lazy val placeholders: Set[PlaceholderVar] = parts.flatMap(_.placeholders)

  /**
    * Check whether dropping the given var would put one or missing constraints out of scope---thus meaning the entailment can never become true.
    */
  private def tryingToDropVarsWithMissingConstraints(varsToDrop: Seq[Var]): Boolean = {
    (missingPureConstraints.flatMap(_.getNonNullVars) intersect varsToDrop.toSet).nonEmpty
  }

  def dropVars(varsToDrop: Seq[Var]): Option[ExtensionType] = {
    if (varsToDrop.isEmpty) {
      Some(this)
    } else {
      logger.debug(s"Will remove bound variables ${varsToDrop.mkString(",")} from extensionType")
      if (tryingToDropVarsWithMissingConstraints(varsToDrop)) {
        // We're forgetting variables for which there are still missing constraints
        // After dropping, it will no longer be possible to supply the constraints
        // We hence discard the extension type
        logger.debug(s"Discarding extension type: Missing pure constraints $missingPureConstraints contain at least one discarded variable from ${varsToDrop.mkString(", ")}")
        None
      } else {
        // From the pure constraints, we must remove the variables completely, because after forgetting a variable,
        // the (ensured) constraints become meaningless for the context...
        val partsAfterDroppingPureConstraints = parts map (_.dropVarsFromPureConstraints(varsToDrop.toSet))

        // ...whereas in the interface nodes/usage info, we replace the bound vars by placeholders
        // TODO: More efficient variable dropping for extension types
        // Rename the vars to fresh placeholder vars
        val maxPlaceholder = PlaceholderVar.maxIndex(placeholders)
        val newPlaceholders = (1 to varsToDrop.size) map (i => PlaceholderVar(maxPlaceholder + i))
        val pairs: Seq[(Var, Var)] = varsToDrop.zip(newPlaceholders.map(_.toFreeVar))
        val replaceByPlacheolders: SubstitutionUpdate = SubstitutionUpdate.fromPairs(pairs)


        // Note: Must to normalization to get rid of gaps in results and strange order by doing normalization
        val partsAfterDropping = partsAfterDroppingPureConstraints map (_.updateSubst(replaceByPlacheolders, convertToNormalform = true))

        Some(ExtensionType(partsAfterDropping))
      }
    }
  }

  def updateSubst(f: SubstitutionUpdate): ExtensionType = {
    ExtensionType(parts map (_.updateSubst(f, convertToNormalform = false)))
  }

  def compositionOptions(other: ExtensionType): Seq[ExtensionType] = {
    for {
      composed <- CanCompose.compositionOptions(parts.toSeq ++ other.parts)
      if composed.forall(_.hasConsistentPureConstraints)
    } yield ExtensionType(composed)
  }

//  def compose(other: ExtensionType): Option[ExtensionType] = {
//    val composed = CanCompose.composeAll(parts.toSeq ++ other.parts)
//    if (composed.forall(_.hasConsistentPureConstraints))
//      Some(ExtensionType(composed))
//    else {
//      logger.debug(s"Discarding extension type $composed because of inconsistent pure constraints")
//      None
//    }
//  }

}

object ExtensionType {

  implicit val etypeToLatex: ToLatex[ExtensionType] = EntailmentInstanceToLatex.etypeToLatex

  def apply(parts: Seq[TreeInterface]): ExtensionType = ExtensionType(parts.toSet)

}
