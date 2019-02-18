package at.forsyte.harrsh

import at.forsyte.harrsh.main.EntailmentQuery
import at.forsyte.harrsh.util.ToLatex._

package object converters {

  case class ConversionException(msg: String) extends Exception

  def sanitize(s: String): String = s.replace('α', 'y').replaceAllLiterally("null","nil")

  trait EntailmentFormatConverter extends ((String, EntailmentQuery) => Seq[(String,String)])

  val ToLatexConverter: EntailmentFormatConverter = (filename: String, pr: EntailmentQuery) => Seq((filename+".tex", pr.toLatex))

}
