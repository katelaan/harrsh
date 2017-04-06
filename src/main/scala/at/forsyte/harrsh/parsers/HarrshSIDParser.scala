package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.parsers.buildingblocks.{Atoms, QuantifierPrefix}

/**
  * Created by jkatelaa on 10/20/16.
  */
private[parsers] trait HarrshSIDParser extends SIDCombinatorParser with HarrshLogging {

  self : Atoms with QuantifierPrefix =>

  override def parseSID : Parser[SID] = rep1sep(parseRule, ";") <~ opt(";") ^^ {
    rules =>
      val startPred : String = rules.head.head
      val maxNumFV : Int = rules.map(_.freeVars.size).max
      val desc : String = startPred + "-SID"
      SID(startPred, rules, desc, maxNumFV)
  }

  // TODO This is still somewhat brittle, in that the parser does not detect if the largest free variable of this rule is less than the max free var for other rules of the same predicate, thus erroneously assuming an arity that is too low
  def parseRule : Parser[Rule] = parseHead ~ ("<=" ~> parseBody) ^^ {
    case head ~ body =>
      val (renamedBody, filledFreeVars, boundVars) = body.toSymbolicHeap
      logger.debug("Assembling rule out of head " + head + " and body " + body + " yielding modified body " + renamedBody)
      Rule(head, filledFreeVars, boundVars, renamedBody)
  }

  private def parseHead : Parser[String] = ident

  override def parseBody : Parser[StringSymbolicHeap] = parseQuantifiers ~> parseSpatial ~ opt(":" ~> parsePure) ^^ {
    // Note that we're ignoring the parse result of the quantifier prefix & instead introduce bound variables automatically
    case spatial ~ pure => StringSymbolicHeap(pure.getOrElse(Seq()), spatial)
  }

  private def parseSpatial : Parser[Seq[StringSpatialAtom]] = rep1sep(parseSpatialAtom, "*")

  private def parsePure : Parser[Seq[StringPureAtom]] = "{" ~> rep1sep(parsePureAtom, ",") <~ "}"

}
