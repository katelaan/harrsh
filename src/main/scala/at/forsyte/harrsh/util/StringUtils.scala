package at.forsyte.harrsh.util

import java.text.SimpleDateFormat
import java.util.Calendar

import scala.annotation.tailrec

object StringUtils {

  @tailrec def literalReplacements(reps : Seq[(String,String)], s : String) : String = {
    if (reps.isEmpty) s else literalReplacements(reps.tail, s.replaceAllLiterally(reps.head._1, reps.head._2))
  }

  private def computeColumnLength(entries : Seq[Any], minLength : Int) : Int = Math.max(minLength, (""+:entries).map(_.toString.size).max + 1)

  sealed trait ColAlignment {
    def alignRight: Boolean = this match {
      case AlignRight => true
      case AlignLeft => false
    }
    def alignLeft: Boolean = !alignRight
  }
  case object AlignRight extends ColAlignment
  case object AlignLeft extends ColAlignment

  case class TableConfig(headings: Seq[String], minColLengths: Seq[Int], colAlignment: Seq[ColAlignment]) {
    if (Set(headings.length, minColLengths.length, colAlignment.length).size != 1) {
      throw new IllegalArgumentException("Inconsistent lengths in table config")
    }
  }

  def defaultTableConfigForHeadings(headings: Seq[String]): TableConfig = {
    val colLengths = headings map (_.size + 1)
    val columnAlignment = Seq.fill(headings.length)(AlignRight)
    TableConfig(headings, colLengths, columnAlignment)
  }

  def toTable(config: TableConfig, entries: Seq[Seq[String]]) : String = {
    val colLengths = for {
      (minLength, ix) <- config.minColLengths.zipWithIndex
    } yield computeColumnLength(entries.map(line => line(ix)), minLength)

    val hrule = delimLine(colLengths)

    val withLayout = (row: Seq[String]) => (row, colLengths, config.colAlignment).zipped.toSeq

    val lines = for {
      entry <- entries
    } yield inColumns(withLayout(entry))

    hrule + "\n" + inColumns(withLayout(config.headings)) + "\n" + hrule + "\n" + lines.mkString("\n")+"\n" + hrule
  }

  private def inColumns(cols : Seq[(String,Int,ColAlignment)]) : String = {
    if (cols.isEmpty) "|" else {
      val (content, width, align) = cols.head
      val padding = " "*Math.max(0,width - content.length)
      val leftPadding = if (align.alignRight) padding else ""
      val rightPadding = if (align.alignLeft) padding else ""
      "|" + leftPadding + content + rightPadding + inColumns(cols.tail)
    }
  }
  private def delimLine(cols : Seq[Int]): String = "+" + "-"*(cols.sum+cols.size-1) + "+"

  def indexifyNumbers(s : String) : String = {
    val (indexified,lastChar) = s.foldLeft[(String,Char)](("",'?')){
      case ((s,prev), curr) => (prev.isDigit, curr.isDigit) match {
        case (false, true) => (s + "_{" + curr, curr)
        case (true, false) => (s + "}" + curr, curr)
        case _ => (s + curr, curr)
      }
    }
    if (lastChar.isDigit) indexified + '}' else indexified
  }

  def splitOffExtension(fileName: String): Option[(String, String)] = {
    fileName.lastIndexOf('.') match {
      case -1 => None
      case i =>
        val (file, ext) = fileName.splitAt(i)
        Some(file, ext.tail)
    }
  }

  def indent(numSpaces: Int)(s: String) = s.lines.map((" "*numSpaces) + _).mkString("\n")

  def today(): String = {
    val now = Calendar.getInstance().getTime()
    val format = new SimpleDateFormat("yyyy-MM-dd")
    format.format(now)
  }

}
