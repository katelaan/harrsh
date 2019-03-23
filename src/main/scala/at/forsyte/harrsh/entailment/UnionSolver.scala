package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.UnionProfile.ProfileIdTag
import at.forsyte.harrsh.seplog.inductive.RichSid

object UnionSolver extends TopLevelSolver {

  // TODO: After each composition step, forget all bound vars that don't occur later

  override def checkValidityOfInstantiation(entailmentInstance: EntailmentInstance, maybeLhsPureProfile: Option[EntailmentProfile], renamedCallProfileSets: Seq[Set[EntailmentProfile]]): Boolean = {
    assert(renamedCallProfileSets forall (_.nonEmpty))
    assert(renamedCallProfileSets forall(ps => ps.forall(_.isDecomposable)))

    val sid = entailmentInstance.rhs.sid
    val lhsConstraint = entailmentInstance.lhs.topLevelConstraint
    val rhsConstraint = entailmentInstance.rhs.topLevelConstraint

    val (singletonCallProfileSets, nonSingletonProfileSets) = renamedCallProfileSets.partition(_.size == 1)
    val singletonProfiles = maybeLhsPureProfile.toSeq ++ singletonCallProfileSets.map(_.head)
    val combinedSingletonProfile = combineSingletonProfiles(sid, singletonProfiles)
    val indexedNonSingletonProfiles: Seq[Set[(EntailmentProfile, Int)]] = nonSingletonProfileSets map {
      ps => ps.zipWithIndex
    }
    val combinedNonSingletonProfile = if (combinedSingletonProfile.isFailure || indexedNonSingletonProfiles.isEmpty) {
      NothingToCompose
    } else {
      combineNonSingletonProfiles(sid, indexedNonSingletonProfiles)
    }
    val unionProfileForComposition = compose(sid, combinedSingletonProfile, combinedNonSingletonProfile)
    unionProfileForComposition match {
      case None =>
        logger.debug("No combined profile => LHS unsat => Entailment holds")
        true
      case Some(profile) =>
        logger.debug("Composed union profile (to which we will apply merge and forget operations):\n" + profile)
        val processed = profile.mergeUsingNonProgressRules(sid).forget(lhsConstraint.boundVars)
        logger.debug("Final union profile:\n" + processed)
        checkValidityOf(processed, sid, rhsConstraint)
    }
  }

  private def checkValidityOf(finalUnionProfile: UnionProfile, sid: RichSid, rhsConstraint: TopLevelConstraint): Boolean = {
    val finalDecomps = finalUnionProfile.taggedDecomps.filterKeys{
      _.isFinal(sid, rhsConstraint)
    }
    logger.info("Final decompositions in union profile are:\n" + finalDecomps.mkString("\n"))
    if (finalDecomps.isEmpty) {
      logger.debug("No final decompositions => Entailment does not hold")
      false
    } else {
      val allTags = finalUnionProfile.taggedDecomps.values.reduce(_.union(_))
      logger.info("Will check if final decompositions subsume all tags in the union profile:\n" + allTags)
      checkUniversality(finalDecomps, allTags)
    }
  }

  private def checkUniversality(finalDecomps: Map[ContextDecomposition, ProfileIdTag], allTags: ProfileIdTag): Boolean = {
    val tagOfAllFinalDecomps = finalDecomps.values.reduce(_.union(_))
    // TODO: For long sequences it would be more efficient to compute the universality of the constraints directly rather than checking the membership for every single vector
    allTags.singleTagStream forall { tag =>
      val res = tagOfAllFinalDecomps.contains(tag)
      if (!res) logger.debug("Combined profile is *not* final for tag $tag => Entailment does not hold")
      res
    }
  }

  private def singleTags(numberOfProfiles: Seq[Int]): Stream[Seq[Int]] = {
    if (numberOfProfiles.isEmpty) {
      Stream(Seq.empty)
    } else {
      for {
        tail <- singleTags(numberOfProfiles.tail)
        i <- 0 until numberOfProfiles.head
      } yield i +: tail
    }
  }

  private def compose(sid: RichSid, fst: CompositionResult[EntailmentProfile], snd: CompositionResult[UnionProfile]): Option[UnionProfile] = (fst, snd) match {
    case (CompositionFailed, _) => None
    case (_, CompositionFailed) => None
    case (NothingToCompose, right) => right.toOption
    case (left, NothingToCompose) => left.map(mkTrivialUnion).toOption
    case (SuccessfulComposition(fst), SuccessfulComposition(snd)) =>
      logger.debug(s"Will compute union of singleton profile\n$fst\nand union profile\n$snd")
      mkTrivialUnion(fst).compose(sid, snd)
  }

  private def mkTrivialUnion(profile: EntailmentProfile): UnionProfile = {
    val trivialMap = profile.decompsOrEmptySet.zip(Stream.continually(ProfileIdTag.empty)).toMap
    UnionProfile(trivialMap, profile.params)
  }

  private def combineSingletonProfiles(sid: RichSid, singletonProfiles: Seq[EntailmentProfile]): CompositionResult[EntailmentProfile] = {
    if (singletonProfiles.nonEmpty) {
      logger.debug("Will combine all singleton profiles")
     EntailmentProfileComposition.composeAll(sid, singletonProfiles) match {
       case None => CompositionFailed
       case Some(p) => SuccessfulComposition(p)
     }
    } else {
      NothingToCompose
    }
  }

  private def combineNonSingletonProfiles(sid: RichSid, nonSingletonProfileSets: Seq[Set[(EntailmentProfile, Int)]]): CompositionResult[UnionProfile] = {
    val unionProfiles = nonSingletonProfileSets map toUnionProfile
    logger.debug(s"Initial union profiles:\n${unionProfiles.mkString("\n")}")
    composeAll(sid, unionProfiles) match {
      case None => CompositionFailed
      case Some(profile) => SuccessfulComposition(profile)
    }
  }

  private def composeAll(sid: RichSid, ups: Seq[UnionProfile]): Option[UnionProfile] = {
    ups.tail.foldLeft[Option[UnionProfile]](Some(ups.head)){
      case (maybeFstUp, sndUp) => maybeFstUp.flatMap(_.compose(sid, sndUp))
    }
  }

  private def toUnionProfile(profiles: Set[(EntailmentProfile, Int)]): UnionProfile = {
    val indexedDecomps = profiles.flatMap(addIndexToDecomps)
    val taggedDecomps = mergeIdentical(indexedDecomps)
    val decompsTaggedWithSeq = taggedDecomps mapValues ProfileIdTag.fromSingleEntry
    val sharedConstraints = profiles.head._1.sharedConstraints
    val params = profiles.head._1.params
    UnionProfile(decompsTaggedWithSeq, params)
  }

  private def addIndexToDecomps(indexedProfile: (EntailmentProfile, Int)): Set[(ContextDecomposition, Int)] = {
    indexedProfile._1.decompsOrEmptySet zip Stream.continually(indexedProfile._2)
  }

  private def mergeIdentical(indexedDecomps: Set[(ContextDecomposition, Int)]): Map[ContextDecomposition, Set[Int]] = {
    val grouped = indexedDecomps.groupBy(_._1)
    grouped.mapValues(_.map(_._2))
  }

  sealed trait CompositionResult[+A] {

    def isFailure: Boolean = this match {
      case CompositionFailed => true
      case _ => false
    }

    def map[B](f: A => B): CompositionResult[B] = this match {
      case CompositionFailed => CompositionFailed
      case NothingToCompose => NothingToCompose
      case SuccessfulComposition(p) => SuccessfulComposition(f(p))
    }

    def toOption: Option[A] = this match {
      case CompositionFailed => None
      case NothingToCompose => throw new IllegalStateException
      case SuccessfulComposition(p) => Some(p)
    }

  }
  case object CompositionFailed extends CompositionResult[Nothing]
  case object NothingToCompose extends CompositionResult[Nothing]
  case class SuccessfulComposition[A](result: A) extends CompositionResult[A]

}
