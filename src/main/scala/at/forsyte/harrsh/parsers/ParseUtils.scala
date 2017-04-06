package at.forsyte.harrsh.parsers

/**
  * Created by jens on 2/25/17.
  */
object ParseUtils {

  def stripCommentLines(input : String, commentPrefix : String) = {
    val lines = input.split("\n")
    val strippedLines = lines.filterNot(_.startsWith(commentPrefix))
    strippedLines.mkString("\n")
  }

}
