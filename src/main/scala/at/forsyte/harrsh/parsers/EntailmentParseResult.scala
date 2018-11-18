package at.forsyte.harrsh.parsers

import scala.collection.SortedSet
import at.forsyte.harrsh.seplog.BoundVar
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

case class EntailmentParseResult(lhs: SymbolicHeap, rhs: SymbolicHeap, sid: SID, entailmentHolds: Option[Boolean])

object EntailmentParseResult {

  case class ConversionException(msg: String) extends Exception

  def toSlideFormat(filename: String, pr: EntailmentParseResult): Seq[(String,String)] = {
    val lhsSid = EntailmentParsers.extractSidForCalls(pr.sid, pr.lhs.predCalls.toSet).getOrElse(SID.empty)
    val rhsSid = EntailmentParsers.extractSidForCalls(pr.sid, pr.rhs.predCalls.toSet).getOrElse(SID.empty)
    val lhsFile = filename + ".lhs.pred"
    val rhsFile = filename + ".rhs.pred"
    Seq(
      (lhsFile, toSlideSid(pr.lhs, lhsSid)),
      (rhsFile, toSlideSid(pr.rhs, rhsSid)),
      (filename + ".callargs", s"$lhsFile $rhsFile")
    )
  }

  private def toSlideSid(toplevel: SymbolicHeap, sid: SID): String = {
    val callString = slideRootCall(toplevel)
    val preds = sid.preds map toSlidePred
    (callString +: preds).mkString("\n\n")
  }

  private def toSlidePred(pred: Predicate): String = {
    val slideRules = pred.bodySHs map shToSlideSh
    s"${pred.defaultCall} ::= ${slideRules.mkString(" | ")}"
  }

  private def shToSlideSh(sh: SymbolicHeap): String = {
    val prefix = if (sh.boundVars.isEmpty) "" else {
      s"\\E ${sh.boundVars.mkString(",")} . "
    }
    val ptrs = sh.pointers map {
      case PointsTo(from, to) => s"$from->${to.mkString(",")}"
    }
    val calls = sh.predCalls map (_.toString)
    val pure = sh.pure map {
      // TODO: Do they assume implicit disequalities or not?
      case PureAtom(l, r, isEquality) => l.toString + (if (isEquality) "=" else "!=") + r
    }
    // TODO: The delimiter between pure formulas is just guess work
    val pureSuffix = if (pure.isEmpty) "" else " & " + pure.mkString(", ")
    val spatial = (ptrs ++ calls).mkString(" * ")
    val spatialOrEmp = if (spatial.isEmpty) "emp" else spatial
    val shString = prefix + spatialOrEmp + pureSuffix
    sanitize(shString)
  }

  private def sanitize(s: String): String = s.replace('α', 'y').replaceAllLiterally("null","nil")

  private def slideRootCall(toplevel: SymbolicHeap): String = s"RootCall ${shToSlideSh(toplevel)}"

  val SbExt = "sb"
  val SbType = "node"
  val SbFieldPrefix = "f"

  def toSongbirdFormat(filename: String, pr: EntailmentParseResult): Seq[(String,String)] = {
    try {
      val typeDecl = toSongbirdType(pr.sid, pr.lhs, pr.rhs)
      val preds = pr.sid.preds map predToSongbird
      val query = toplevelSongbirdQuery(pr.lhs, pr.rhs)
      val content = typeDecl + "\n\n" + preds.mkString("\n\n") + "\n\n" + query
      Seq((filename + '.' + SbExt, content))
    } catch {
      case ConversionException(msg) =>
        println("WARNING: Conversion failed with exception: " + msg)
        Seq.empty
    }
  }

  private def predToSongbird(pred: Predicate): String = {
    val rules = pred.bodySHs map shToSongbird
    "pred " + pred.defaultCall + " := " + rules.mkString("\n \\/    ") + ';'
  }

  private def toplevelSongbirdQuery(lhs: SymbolicHeap, rhs: SymbolicHeap): String = {
    "checkentail " + shToSongbird(lhs) + " |- " + shToSongbird(rhs) + ';'
  }

  private def shToSongbird(sh: SymbolicHeap): String = {
    val ptrs = sh.pointers map ptoToSongbird
    val calls = sh.predCalls map callToSongbird
    val spatial = (ptrs ++ calls).mkString(" * ")
    val pure = (sh.pure map pureToSongbird).mkString(" & ")
    val firstAmp = if (pure.isEmpty || spatial.isEmpty) "" else " & "
    val qfree = spatial + firstAmp + pure
    val quantified = quantifyForSongbird(sh.boundVars, qfree)
    sanitize(quantified)
  }

  private def ptoToSongbird(pto: PointsTo): String = pto.from.toString + "->" + SbType + '{' + pto.to.mkString(",") + '}'

  private def pureToSongbird(pure: PureAtom): String = pure.l.toString + (if (pure.isEquality) "=" else "!=") + pure.r

  private def callToSongbird(call: PredCall): String = call.toString

  private def quantifyForSongbird(bv: SortedSet[BoundVar], qfree: String): String = {
    if (bv.isEmpty) qfree
    else {
      "(exists " + bv.mkString(",") + ". " + qfree + ")"
    }
  }

  private def toSongbirdType(sid: SID, lhs: SymbolicHeap, rhs: SymbolicHeap): String = {
    val rhsSidSizes = for {
      p <- sid.preds.toSet[Predicate]
      r <- p.bodySHs
      ptr <- r.pointers
    } yield ptr.to.length
    val rhsQuerySizes = for {
      sh <- Seq(lhs, rhs)
      ptr <- sh.pointers
    } yield ptr.to.length
    val rhsSizes = rhsSidSizes ++ rhsQuerySizes
    if (rhsSizes.size > 1)
      throw ConversionException("Multiple pointer arities within one benchmark not supported by Songbird exporter: " + rhsSizes)
    if (rhsSizes.size == 1) {
      val fields = (1 to rhsSizes.head) map (i => SbFieldPrefix + i)
      val fieldDecls = fields map (f => "    " + SbType + ' ' + f + ';')
      "data " + SbType + " {\n" + fieldDecls.mkString("\n") + "\n};"
    } else {
      ""
    }
  }

  implicit val parseResultToLatex: ToLatex[EntailmentParseResult] = (epr: EntailmentParseResult, naming: Naming) => {
    val query = "Check entailment $" + epr.lhs.toLatex(naming) + " \\models " + epr.rhs.toLatex(naming) + "$"
    val sid = epr.sid.toLatex(naming)
    query + "\n%\n" + "w.r.t.\n%\n" + sid + "\n"
  }

}
