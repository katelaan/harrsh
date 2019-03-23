package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.{HarrshCache, UnboundedCache}

object EntailmentProfileComposition extends HarrshLogging {

  val UseLogSplit: Boolean = false
  val CompositionFunction: (RichSid, Seq[Set[ContextDecomposition]]) => Set[ContextDecomposition] = {
    if (UseLogSplit) allPossibleDecompCompositionsLogSplit else allPossibleDecompCompositions
  }

  def composeAll(sid: RichSid, profiles: Seq[EntailmentProfile]): Option[EntailmentProfile] = {
    logger.debug(s"Will compose the following ${profiles.size} profiles:\n" + profiles.mkString("\n"))
    assert(profiles forall (_.isConsistentWithFocus(sid)))
    for {
      combinedConstraints <- VarConstraints.combineEnsured(profiles.map(_.sharedConstraints))
      composed <- composeWithSharedConstraints(sid, profiles, combinedConstraints)
    } yield composed
  }

  private def composeWithSharedConstraints(sid: RichSid, profiles: Seq[EntailmentProfile], newSharedConstraints: VarConstraints): Option[EntailmentProfile] = {
    if (profiles.forall(_.isDecomposable)) {
      composeDecomps(sid, profiles, newSharedConstraints)
    } else {
      logger.debug(s"Short-circuiting (propagating sink state) profile composition of:\n${profiles.mkString("\n")}")
      Some(ProfileOfNondecomposableModels(newSharedConstraints, paramUnion(profiles)))
    }
  }

  private def paramUnion(profiles: Seq[EntailmentProfile]): Set[Var] = profiles.toSet[EntailmentProfile].flatMap(_.params)

  private def composeDecomps(sid: RichSid, profiles: Seq[EntailmentProfile], newSharedConstraints: VarConstraints): Option[EntailmentProfile] = {
    val composedDecomps = CompositionFunction(sid, profiles map (_.decompsOrEmptySet))
    if (composedDecomps.isEmpty) {
      logger.debug(s"Composed profiles are contradictory => No composition result")
      None
    } else {
      val res = ProfileOfDecomps(composedDecomps, newSharedConstraints, paramUnion(profiles))
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

  private def groupsOfTwo[A](as: Seq[A]): Seq[Seq[A]] = {
    if (as.isEmpty) Seq.empty
    else if (as.tail.isEmpty) Seq(as)
    else as.take(2) +: groupsOfTwo(as.drop(2))
  }

  private def allPossibleDecompCompositionsLogSplit(sid: RichSid, decompsBySource: Seq[Set[ContextDecomposition]]): Set[ContextDecomposition] = {
    decompsBySource.size match {
      case 1 => decompsBySource.head
      case 2 => profileCompositionCache((sid, decompsBySource.head, decompsBySource.tail.head))
      case size =>
        assert(size > 2)
        val grouped = groupsOfTwo(decompsBySource)
        val composedGroups = grouped map (allPossibleDecompCompositionsLogSplit(sid, _))
        allPossibleDecompCompositionsLogSplit(sid, composedGroups)

//        val (left, right) = decompsBySource.splitAt(size / 2)
//        logger.info(s"Split ${decompsBySource.size} profiles into sequences of length ${left.size} and ${right.size}")
//        val leftComposition = allPossibleDecompCompositionsLogSplit(sid, left)
//        val rightComposition = allPossibleDecompCompositionsLogSplit(sid, right)
//        profileCompositionCache((sid, leftComposition, rightComposition))
    }

    if (decompsBySource.tail.isEmpty) {
      decompsBySource.head
    } else {
      val partialComposition = profileCompositionCache((sid, decompsBySource.head, decompsBySource.tail.head))
      allPossibleDecompCompositions(sid, partialComposition +: decompsBySource.drop(2))
    }
  }


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
