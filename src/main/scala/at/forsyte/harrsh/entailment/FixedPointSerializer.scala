package at.forsyte.harrsh.entailment

case class FixedPointSerializer(ei: EntailmentInstance) {

  private val sid = ei.rhs.sid
  private val rhsTopLevelConstraint = ei.rhs.topLevelConstraint

  def apply(statesByPred: Map[String, Set[EntailmentProfile]]): String = {
    val lines = Stream("FIXED_POINT {") ++ statesByPred.toStream.flatMap(pair => serializePred(pair._1, pair._2)).map(indent) ++ Stream("}")
    lines.mkString("\n")
  }

  def apply(profss: Iterable[Set[EntailmentProfile]]): String = {
    val lines = Stream("SEQUENCE_OF_PROFILE_SETS {") ++ profss.toStream.flatMap(serializeSetOfProfiles).map(indent) ++ Stream("}")
      lines.mkString("\n")
  }

  private def indent(s : String) = "  " + s

  private def serializePred(pred: String, profs: Set[EntailmentProfile]): Stream[String] = {
    Stream(s"PRED $pred {") ++ profs.toStream.flatMap(serializeProfile).map(indent) ++ Stream("}")
  }

  private def serializeSetOfProfiles(profs: Set[EntailmentProfile]) = {
    Stream("SET_OF_PROFILES {") ++ profs.toStream.flatMap(serializeProfile).map(indent) ++ Stream("}")
  }

  private def serializeProfile(profile: EntailmentProfile): Stream[String] = {
    (Stream("PROFILE {",
      s"  FVS: ${profile.orderedParams.mkString(", ")}")
      ++ Some("  ACCEPTING").filter(_ => profile.isFinal(sid, rhsTopLevelConstraint))
      ++ Some("  SHARED: " + profile.sharedConstraints)
      ++ serializeContent(profile) ++ Stream("}"))
  }

  private def serializeContent(profile: EntailmentProfile) = profile match {
    case ProfileOfDecomps(decomps, _, _) => serializeDecomps(decomps)
    case ProfileOfNondecomposableModels(constraints, _) =>
      Stream(indent("NO CONSISTENT DECOMPOSITION"), indent(constraints.toString))
  }

  private def serializeDecomps(decomps: Set[ContextDecomposition]): Stream[String] = {
    decomps.toStream.flatMap(serializeContextDecomposition).map(indent)
  }

  private def serializeContextDecomposition(decomp: ContextDecomposition): Stream[String] = {
    val decompStrs = decomp.parts.toList map (_.toString)
    val indent = "       "
    val fst = if (decompStrs.nonEmpty) decompStrs.head else "empty"
    val tail = if (decompStrs.nonEmpty) decompStrs.tail else Seq.empty
    val constraintsStr = indent + decomp.constraints
    Stream(s"Decomp($fst,") ++ tail.map(indent + _ + ",") ++ Stream(constraintsStr + ")")
  }

}
