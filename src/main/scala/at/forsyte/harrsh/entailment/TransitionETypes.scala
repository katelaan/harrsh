package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

sealed trait TransitionETypes {
  def toTarget: Option[EntailmentAutomaton.State]
}

case class UnmatchableLocalAllocation(lab: SymbolicHeap) extends TransitionETypes with HarrshLogging {
  override def toTarget: Option[EntailmentAutomaton.State] = {
    logger.debug(s"No extension types for local allocation $lab => Return inconsistent state")
    Some(EntailmentAutomaton.State(Set.empty, lab.freeVars))
  }
}

case object InconsistentTransitionSources extends TransitionETypes with HarrshLogging {
  override def toTarget: Option[EntailmentAutomaton.State] = {
    logger.debug(s"Instantiation of transition sources is inconsistent => No target state")
    None
  }
}

case class ConsistentTransitionETypes(etypes: Seq[Set[ExtensionType]], lab: SymbolicHeap) extends TransitionETypes with HarrshLogging {

  override def toTarget: Option[EntailmentAutomaton.State] = {
    val targetETs = allPossibleETCompositions(etypes, lab)
    logger.debug(s"Target state:\n${if (targetETs.nonEmpty) targetETs.mkString("\n") else "empty (sink state)"}")
    Some(EntailmentAutomaton.State(targetETs, lab.freeVars))
  }

  private def allPossibleETCompositions(etsByState: Seq[Set[ExtensionType]], lab: SymbolicHeap): Set[ExtensionType] = {
    for {
      ets <- Combinators.choices(etsByState map (_.toSeq)).toSet[Seq[ExtensionType]]
      combined <- tryToCombineETs(ets, lab)
    } yield combined
  }

  private def tryToCombineETs(ets: Seq[ExtensionType], lab: SymbolicHeap): Option[ExtensionType] = {
    logger.debug(s"Trying to combine:\n${ets.map(_.parts.mkString("ET(", "\n  ", ")")).mkString("\n")}\nw.r.t. symbolic heap $lab...")
    val res = for {
      combined <- composeAll(ets)
      _ = logger.debug(s"Resulting parts of the ET:\n${combined.parts.mkString("\n")}")
      // Bound variables are not visible from outside the transition, so we remove them from the extension type
      restrictedToFreeVars <- restrictToFreeVars(combined)
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

  private def composeAll(ets: Seq[ExtensionType]): Option[ExtensionType] = {
    if (ets.size <= 1) ets.headOption else {
      for {
        combinedHead <- ets.head compose ets.tail.head
        allComposed <- composeAll(combinedHead +: ets.drop(2))
      } yield allComposed
    }
  }

  private def restrictToFreeVars(etype: ExtensionType): Option[ExtensionType] = {
    logger.debug(s"Bound vars in result: ${etype.boundVars} (in the symbolic heap: ${lab.boundVars.mkString(",")})")
    assert(etype.boundVars.diff(lab.boundVars).isEmpty,
      s"Extension type $etype contains bound vars not in symbolic heap $lab")
    etype.dropVars(lab.boundVars.toSeq)
  }

}

object TransitionETypes {

  def apply(src: Seq[EntailmentAutomaton.State], lab: SymbolicHeap, sid: SID): TransitionETypes = {
    val instantiatedETs = InstantiatedSourceStates(src, lab)
    if (instantiatedETs.isConsistent) {
      val local = LocalETs(lab, sid)
      combineLocalAndSourceEtypes(local, instantiatedETs, lab)
    } else {
      InconsistentTransitionSources
    }
  }

  private def combineLocalAndSourceEtypes(local: LocalETs, instantiatedETs: InstantiatedSourceStates, lab: SymbolicHeap) = {
    if (local.areDefined) {
      ConsistentTransitionETypes(local +: instantiatedETs, lab)
    } else {
      UnmatchableLocalAllocation(lab)
    }
  }

}