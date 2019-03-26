package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.UnionProfile.ProfileIdTag
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.RichSid

import scala.collection.mutable

case class UnionProfile(taggedDecomps: Map[ContextDecomposition, ProfileIdTag], params: Set[Var]) extends HarrshLogging {

  def compose(sid: RichSid, other: UnionProfile): Option[UnionProfile] = {
    val composedDecomps = UnionProfile.composeAllDecomps(sid, taggedDecomps, other.taggedDecomps)
    if (composedDecomps.isEmpty) {
      logger.debug(s"Composed profiles are contradictory => No composition result")
      None
    } else {
      val res = UnionProfile(composedDecomps, params ++ other.params)
      logger.debug(s"Profile composition result:\n$res")
      Some(res)
    }
  }

  def postProcess(boundVars: Set[Var], sid: RichSid): UnionProfile = {
    mergeUsingNonProgressRules(sid).forget(boundVars)
  }

  private def applyToAllDecomps(f: ContextDecomposition => Iterable[ContextDecomposition]): UnionProfile = {
    val afterApplication = taggedDecomps.toSeq.flatMap{
      pair =>
        val closedDecomps = f(pair._1)
        closedDecomps.zip(Stream.continually(pair._2))
    }
    UnionProfile(afterApplication, params)
  }

  def mergeUsingNonProgressRules(sid: RichSid): UnionProfile = {
    applyToAllDecomps(MergeUsingNonProgressRules.useNonProgressRulesToMergeContexts(_, sid))
  }

  def forget(varsToForget: Set[Var]): UnionProfile = {
    applyToAllDecomps(_.forget(varsToForget)).copy(
      params = params filterNot varsToForget
    )
  }

  override def toString: String = {
    val decompStr = taggedDecomps.map(pair => pair._1 + " --> " + pair._2).mkString("\n  ")
    s"""UnionProfile(
       |  params = ${params.toSeq.sorted.mkString(", ")}
       |  $decompStr
       |)""".stripMargin
  }

}

object UnionProfile {

  def apply(unmergedTaggedDecomps: Seq[(ContextDecomposition, ProfileIdTag)], params: Set[Var]): UnionProfile = {
    UnionProfile(groupResults(unmergedTaggedDecomps), params)
  }

  type ProductIdTag = Seq[Set[Int]]

  case class ProfileIdTag(underlying: Set[ProductIdTag]) {

    assert(underlying.map(_.length).size == 1,
      "All tags must have the same length; that's not the case for " + this)

    def concat(other: ProfileIdTag): ProfileIdTag = {
      ProfileIdTag(for {
        left <- underlying
        right <- other.underlying
      } yield left ++ right)
    }

    def union(other: ProfileIdTag): ProfileIdTag = {
      ProfileIdTag(underlying ++ other.underlying)
    }

    def contains(tag: Seq[Int]): Boolean = {
      underlying exists (subsumes(_, tag))
    }

    private def subsumes(productTag: ProductIdTag, tag: Seq[Int]): Boolean = {
      assert(productTag.size == tag.size)
      val zipped = productTag zip tag
      zipped forall (pair => pair._1.contains(pair._2))
    }

    def singleTagStream: Stream[Seq[Int]] = {
      val found: mutable.Set[Seq[Int]] = mutable.Set.empty
      for {
        tag <- underlying.toStream
        singleTag <- singleTagsOf(tag)
        if !found(singleTag)
        _ = found.update(singleTag, included = true)
      } yield singleTag
    }

    private def singleTagsOf(tag: ProductIdTag): Stream[Seq[Int]] = {
      if (tag.isEmpty) {
        Stream(Seq.empty)
      } else {
        for {
          tail <- singleTagsOf(tag.tail)
          hd <- tag.head
        } yield hd +: tail
      }
    }

    override def toString: String = {
      val strs = underlying map {
        seq => seq.map(_.mkString("{",",","}")).mkString("[", ", ", "]")
      }
      strs.mkString("<", "; ", ">")
    }

  }

  object ProfileIdTag {

    def fromSingleEntry(is: Set[Int]): ProfileIdTag = ProfileIdTag(Set(Seq(is)))

    def empty: ProfileIdTag = ProfileIdTag(Set(Seq.empty[Set[Int]]))

  }

  def composeAllDecomps(sid: RichSid, fst: Map[ContextDecomposition, ProfileIdTag], snd: Map[ContextDecomposition, ProfileIdTag]): Map[ContextDecomposition, ProfileIdTag] = {
    val unmergedCompositionResults = allCompositionResults(sid, fst, snd)
    groupResults(unmergedCompositionResults)
  }

  private def allCompositionResults(sid: RichSid, fst: Map[ContextDecomposition, ProfileIdTag], snd: Map[ContextDecomposition, ProfileIdTag]): Seq[(ContextDecomposition, ProfileIdTag)] = {
    for {
      (left, leftTag) <- fst.toSeq
      (right, rightTag) <- snd
      newTag = leftTag concat rightTag
      composed <- ContextDecompositionComposition(sid, left, right)
    } yield (composed, newTag)
  }

  private def groupResults(unmergedCompositionResults: Seq[(ContextDecomposition, ProfileIdTag)]): Map[ContextDecomposition, ProfileIdTag] = {
    val grouped = unmergedCompositionResults.groupBy(_._1)
    grouped.mapValues(_.map(_._2).reduce(_.union(_)))
  }

}
