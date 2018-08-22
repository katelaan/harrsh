package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.PredCall

/**
  * A single abstracted forest, retaining only the tree interfaces rather than the full trees.
  * @param parts Abstracted trees
  */
case class ExtensionType(parts: Set[TreeInterface]) extends HarrshLogging {

  assert(parts forall TreeInterface.isInNormalForm,
    s"Trying to construct extension type from non-normalized parts ${parts.mkString(", ")}")

  override def toString: String = parts.mkString("ET(", ",\n   ", ")")

  def hasNamesForRootParams: Boolean = parts.forall(_.hasNamesForRootParams)

  def isFinal(call: PredCall): Boolean = {
    val res = if (parts.size != 1) {
      // It's not a single (abstracted) tree, so it can't represent a single concrete unfolding tree
      false
    } else {
      val tif = parts.head
      val rootPred = tif.root.pred
      Stream(
        tif.isConcrete, // It represents a concrete tree...
        rootPred.head == call.name, // ...rooted in the correct predicate...
        (call.args, tif.root.subst.toSeq).zipped.forall{
          // ...and with the correct vector of variables at the root (corresponding to the goal predicate call)
          case (arg, substVal) => substVal.contains(arg)
        }
      ).forall(b => b)
    }
    logger.debug(s"Checking whether $this is final w.r.t. $call => $res")
    res
  }

  def nonPlaceholderFreeVars: Set[FreeVar] = parts.flatMap(_.nonPlaceholderFreeVars)

  def placeholdersInSubst: Set[PlaceholderVar] = parts.flatMap(_.placeholders)

  def dropVars(varsToDrop: Seq[Var]): ExtensionType = {
    // TODO: More efficient variable dropping for extension types
    // FIXME: Don't introduce redundant placeholder vars, i.e., if there is a variable alias that we don't drop, we don't need a new placeholder name to avoid empty sets in the substitution
    // Rename the vars to fresh placeholder vars
    val maxPlaceholder = PlaceholderVar.maxIndex(placeholdersInSubst)
    val placeholders = (maxPlaceholder+1 to varsToDrop.size) map (PlaceholderVar(_))
    val pairs: Seq[(Var,Var)] = varsToDrop.zip(placeholders.map(_.toFreeVar))
    val replaceByPlacheolders: SubstitutionUpdate = SubstitutionUpdate.fromPairs(pairs)

    // Get rid of gaps in results and strange order by doing normalization
    val partsAfterDropping = parts map (_.updateSubst(replaceByPlacheolders, convertToNormalform = true))
    ExtensionType(partsAfterDropping)
  }

  def updateSubst(f: SubstitutionUpdate): ExtensionType = {
    ExtensionType(parts map (_.updateSubst(f, convertToNormalform = false)))
  }

  def compose(other: ExtensionType): ExtensionType = {
    val composed = CanCompose.composeAll(parts.toSeq ++ other.parts)
    ExtensionType(composed)
  }

}

object ExtensionType {

  def apply(parts: Seq[TreeInterface]): ExtensionType = ExtensionType(parts.toSet)

}
