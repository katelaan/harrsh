package at.forsyte.harrsh.parsers.buildingblocks

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jens on 4/6/17.
  */
trait UnicodeQuantifierPrefix extends QuantifierPrefix {

  self : JavaTokenParsers =>

  override def parseQuantifiers : Parser[Seq[String]] = rep(parseQuantifier) <~ opt(".")

  private def parseQuantifier : Parser[String] = """\u2203[a-zA-Z_][a-zA-Z0-9_']*""".r map (_.tail) //"\u2203" ~> ident

}
