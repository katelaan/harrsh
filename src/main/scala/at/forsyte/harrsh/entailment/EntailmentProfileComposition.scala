package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.{HarrshCache, UnboundedCache}

object EntailmentProfileComposition extends HarrshLogging {

  def composeAll(sid: RichSid, profiles: Seq[EntailmentProfile], newOrderedParams: Seq[Var]): Option[EntailmentProfile] = {
    logger.debug(s"Will compose the following ${profiles.size} profiles:\n" + profiles.mkString("\n"))
    assert(profiles forall (_.isConsistentWithFocus(sid)))
    for {
      combinedConstraints <- VarConstraints.combineEnsured(profiles.map(_.sharedConstraints))
      composed <- composeWithSharedConstraints(sid, profiles, combinedConstraints, newOrderedParams)
    } yield composed
  }

  private def composeWithSharedConstraints(sid: RichSid, profiles: Seq[EntailmentProfile], newSharedConstraints: VarConstraints, newOrderedParams: Seq[Var]): Option[EntailmentProfile] = {
    if (profiles.forall(_.isDecomposable)) {
      composeDecomps(sid, profiles, newSharedConstraints, newOrderedParams)
    } else {
      logger.debug(s"Short-circuiting (propagating sink state) profile composition of:\n${profiles.mkString("\n")}")
      Some(ProfileOfNondecomposableModels(newSharedConstraints, newOrderedParams))
    }
  }

  private def composeDecomps(sid: RichSid, profiles: Seq[EntailmentProfile], newSharedConstraints: VarConstraints, newOrderedParams: Seq[Var]): Option[EntailmentProfile] = {
    val composedDecomps = allPossibleDecompCompositions(sid, profiles map (_.decompsOrEmptySet))
    if (composedDecomps.isEmpty) {
      logger.debug(s"Composed profiles are contradictory => No composition result")
      None
    } else {
      val res = ProfileOfDecomps(composedDecomps, newSharedConstraints, newOrderedParams)
      logger.debug(s"Profile composition result:\n$res")
      Some(res)
    }
  }

  private type From = (RichSid, Set[ContextDecomposition], Set[ContextDecomposition])
  private type Key = (Set[ContextDecomposition], Set[ContextDecomposition])
  private type Value = Set[ContextDecomposition]

  private val profileCompositionCache: HarrshCache[From, Value] = new UnboundedCache[From, Key, Value](
    "Profile Composition Cache",
    triple => (triple._2, triple._3),
    triple => allPossibleDecompCompositions(triple._1, triple._2, triple._3)
  )

  private def allPossibleDecompCompositions(sid: RichSid, decompsBySource: Seq[Set[ContextDecomposition]]): Set[ContextDecomposition] = {
    if (decompsBySource.tail.isEmpty) {
      decompsBySource.head
    } else {
      val partialComposition = profileCompositionCache((sid, decompsBySource.head, decompsBySource.tail.head))
      allPossibleDecompCompositions(sid, partialComposition +: decompsBySource.drop(2))
    }
  }

  private def allPossibleDecompCompositions(sid: RichSid, fst: Set[ContextDecomposition], snd: Set[ContextDecomposition]): Set[ContextDecomposition] = {
    logger.debug(s"Will compose $fst and $snd")
    val res = for {
      decompOfFst <- fst
      decompOfSnd <- snd
      combined <- decompOfFst.compositionOptions(sid, decompOfSnd)
    } yield combined
    logger.debug(s"Preliminary decomps in profile composition:\n${if (res.nonEmpty) res.mkString("\n") else "empty (sink state)"}")
    res
  }

}
