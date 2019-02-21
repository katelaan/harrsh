package at.forsyte.harrsh.seplog.inductive

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class Sid(override val startPred : String, override val preds : Seq[Predicate], override val description : String) extends SidLike {

  override def toString: String = {
    val predStrings = preds.map(_.toString.lines.map(line => s"    $line ;").mkString("\n"))
    description + " (start predicate '" + startPred + "'): " + predStrings.mkString("\n", "\n", "")
  }

}

object Sid {

  def empty(startPred : String) : Sid = Sid(startPred, Seq.empty[Predicate], "")

  def empty : Sid = Sid("X", Seq.empty[Predicate], "")

}