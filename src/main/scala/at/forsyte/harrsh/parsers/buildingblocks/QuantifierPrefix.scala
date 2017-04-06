package at.forsyte.harrsh.parsers.buildingblocks

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jens on 4/6/17.
  */
trait QuantifierPrefix {

  self : JavaTokenParsers =>

  def parseQuantifiers : Parser[Seq[String]]

}
