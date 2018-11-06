package at.forsyte.harrsh.util

import scala.annotation.tailrec

object StringUtils {

  @tailrec def literalReplacements(reps : Seq[(String,String)], s : String) : String = {
    if (reps.isEmpty) s else literalReplacements(reps.tail, s.replaceAllLiterally(reps.head._1, reps.head._2))
  }

  private def computeColumnLength(entries : Seq[Any], minLength : Int) : Int = Math.max(minLength,entries.map(_.toString.size).max + 1)

  def toTable(headings: Seq[String], minColLengths: Seq[Int], entries: Seq[Seq[String]]) : String = {
    val colLengths = for {
      (minLength, ix) <- minColLengths.zipWithIndex
    } yield computeColumnLength(entries.map(line => line(ix)), minLength)

    val hrule = delimLine(colLengths)

    val lines = for {
      entry <- entries
    } yield inColumns(entry zip colLengths)

    hrule + "\n" + inColumns(headings zip colLengths) + "\n" + hrule + "\n" + lines.mkString("\n")+"\n" + hrule
  }

  private def inColumns(cols : Seq[(String,Int)]) : String = if (cols.isEmpty) "|" else "|" + " "*Math.max(0,cols.head._2 - cols.head._1.length) + cols.head._1 + inColumns(cols.tail)
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

}
