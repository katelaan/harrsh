package at.forsyte.harrsh.parsers.buildingblocks

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jens on 4/6/17.
  */
trait EmptyQuantifierPrefix extends QuantifierPrefix {

  self : JavaTokenParsers =>

  override def parseQuantifiers : Parser[Seq[String]] = success(Seq.empty)

}
