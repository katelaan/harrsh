package at.forsyte.harrsh.converters

import at.forsyte.harrsh.main.EntailmentQuery
import at.forsyte.harrsh.seplog.BoundVar
import at.forsyte.harrsh.seplog.inductive._

import scala.collection.SortedSet

object ToSongbirdFormat extends EntailmentFormatConverter {

  val SbExt = "sb"
  val SbType = "node"
  val SbFieldPrefix = "f"

  override def apply(filename: String, pr: EntailmentQuery): Seq[(String,String)] = {
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
    val ptoArities: Set[Int] = pointerArities(sid, lhs, rhs)
    if (ptoArities.size > 1)
      throw ConversionException("Multiple pointer arities within one benchmark not supported by Songbird exporter: " + ptoArities)
    if (ptoArities.size == 1) {
      val fields = (1 to ptoArities.head) map (i => SbFieldPrefix + i)
      val fieldDecls = fields map (f => "    " + SbType + ' ' + f + ';')
      "data " + SbType + " {\n" + fieldDecls.mkString("\n") + "\n};"
    } else {
      ""
    }
  }

}
