package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.RichSid

case class EntailmentContext private(root: ContextPredCall, calls: Set[ContextPredCall]) {

  lazy val labels: Seq[ContextPredCall] = Seq(root) ++ calls

  lazy val placeholders: Seq[PlaceholderVar] = {
    for {
      l <- labels
      vs <- l.subst.toSeq
      v <- vs
      p <- PlaceholderVar.fromVar(v)
    } yield p
  }

  lazy val placeholdersAsVars: Seq[Var] = {
    for {
      l <- labels
      vs <- l.subst.toSeq
      v <- vs
      if PlaceholderVar.isPlaceholder(v)
    } yield v
  }

  def updateSubst(f: ConstraintUpdater): EntailmentContext = EntailmentContext(root.update(f), calls map (_.update(f)))

  def isConcrete: Boolean = calls.isEmpty

  def rootParamSubsts(sid: RichSid): Seq[Set[Var]] = labels flatMap (_.rootParamSubst(sid))

  def hasNullInRootPosition(sid: RichSid): Boolean = {
    calls.exists(_.hasNullInRootPosition(sid))
  }

  def hasNonNullNamesForRootParams(sid: RichSid): Boolean = rootParamSubsts(sid).forall {
    labelingVars => labelingVars.exists(PlaceholderVar.isNonPlaceholderNonNullFreeVar)
  }

  override def toString: String = {
    val callsString = if (calls.isEmpty) "empty" else calls.mkString(",")
    s"Ctx(root = $root; calls = $callsString)"
  }

}

object EntailmentContext extends HarrshLogging {

  implicit val ordering = Ordering.fromLessThan[EntailmentContext]{
    (left, right) =>
      val (leftLabels, rightLabels) = (left.labels, right.labels)
      val res = leftLabels.size - rightLabels.size match {
        case i if i < 0 => true
        case i if i > 0 => false
        case 0 =>
          val differences = for {
            (l, r) <- (leftLabels zip rightLabels).toStream
            lSmaller = l lessThan r
            rSmaller = (!lSmaller) && (r lessThan l)
            if lSmaller || rSmaller
          } yield lSmaller
          differences.headOption.getOrElse(false)
      }
      logger.trace(s"$left < $right: $res")
      res
  }

  def haveDisjointPlaceholders(ctx1: EntailmentContext, ctx2: EntailmentContext): Boolean = {
    (ctx1.placeholders intersect ctx2.placeholders).isEmpty
  }

}
