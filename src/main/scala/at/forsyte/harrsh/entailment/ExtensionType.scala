package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PredCall, PureAtom}
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

  def hasNamesForRootParams: Boolean = parts.forall(_.hasNamesForRootParams)

  def isFinal(call: PredCall): Boolean = {
    val res = if (parts.size != 1) {
      // Only single (abstracted) trees can be final
      false
    } else {
      val tif = parts.head
      val rootPred = tif.root.pred
      Stream(
        tif.isConcrete, // It represents a concrete tree...
        rootPred.head == call.name, // ...rooted in the correct predicate...
        tif.pureConstraints.missing.isEmpty, // ...without missing pure constraints...
        (call.args, tif.root.subst.toSeq).zipped.forall{
          // ...and with the correct vector of variables at the root (corresponding to the goal predicate call)
          case (arg, substVal) => substVal.contains(arg)
        }
      ).forall(b => b)
    }
    logger.debug(s"Checking whether $this is final w.r.t. $call => $res")
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

  def compose(other: ExtensionType): Option[ExtensionType] = {
    val composed = CanCompose.composeAll(parts.toSeq ++ other.parts)
    if (composed.forall(_.hasConsistentPureConstraints))
      Some(ExtensionType(composed))
    else {
      logger.debug(s"Discarding extension type $composed because of inconsistent pure constraints")
      None
    }
  }

}

object ExtensionType {

  implicit val etypeToLatex: ToLatex[ExtensionType] = EntailmentInstanceToLatex.etypeToLatex

  def apply(parts: Seq[TreeInterface]): ExtensionType = ExtensionType(parts.toSet)

}
