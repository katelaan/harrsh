package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.parsers.buildingblocks.{Atoms, QuantifierPrefix}
import at.forsyte.harrsh.seplog
import at.forsyte.harrsh.seplog.{FreeVar, Var, VarUnNaming}

/**
  * Created by jkatelaa on 10/20/16.
  */
private[parsers] trait HarrshSIDParser extends SIDCombinatorParser with HarrshLogging {

  self : Atoms with QuantifierPrefix =>

  override final def runOnSymbolicHeap(input : String, printFailure : Boolean = true) : Option[SymbolicHeap] = catchNumberFormatException{
    runParser(parseBody)(input, printFailure) map (HarrshSIDParser.stringSHwithHarrshNamingtoSH(_)._1)
  }

  override def parseSID : Parser[SID] = rep1sep(parseRule, ";") <~ opt(";") ^^ {
    rules =>
      val startPred : String = rules.head.head
      val desc : String = startPred + "-SID"
      SID(startPred, rules, desc)
  }

  // TODO This is still somewhat brittle, in that the parser does not detect if the largest free variable of this rule is less than the max free var for other rules of the same predicate, thus erroneously assuming an arity that is too low
  def parseRule : Parser[Rule] = parseHead ~ ("<=" ~> parseBody) ^^ {
    case head ~ body =>
      val (renamedBody, filledFreeVars, boundVars) = HarrshSIDParser.stringSHwithHarrshNamingtoSH(body)
      logger.debug("Assembling rule out of head " + head + " and body " + body + " yielding modified body " + renamedBody)
      Rule(head, boundVars, renamedBody.copy(freeVars = filledFreeVars.map(FreeVar)))
  }

  private def parseHead : Parser[String] = ident <~ opt(paramList)

  private def paramList : Parser[Unit] = ("(" ~> repsep(ident, ",") <~ ")") ^? {
    case ids if ids.forall(HarrshSIDParser.isHarrshFreeVariableString) && ids.map(FreeVar) == HarrshSIDParser.mkAllVars(1 to ids.length) => ()
  }

  override def parseBody : Parser[StringSymbolicHeap] = parseQuantifiers ~> parseSpatial ~ opt(":" ~> parsePure) ^^ {
    // Note that we're ignoring the parse result of the quantifier prefix & instead introduce bound variables automatically
    case spatial ~ pure => StringSymbolicHeap(pure.getOrElse(Seq()), spatial)
  }

  private def parseSpatial : Parser[Seq[StringSpatialAtom]] = rep1sep(parseSpatialAtom, "*")

  private def parsePure : Parser[Seq[StringPureAtom]] = "{" ~> rep1sep(parsePureAtom, ",") <~ "}"

}

object HarrshSIDParser {

  // TODO: Add support for other free variable names

  val FreeVarString = "x"

  private def isHarrshFreeVariableString(fv : String): Boolean = fv match {
    case "null" => true
    case "nil" => true
    case id => id.startsWith(FreeVarString)
  }

  private def mkAllVars(ints : Seq[Int]) : List[Var] = ints.map(i => FreeVar(FreeVarString + i)).toList

  /**
    * Create symbolic heap from string representation assuming that free variables are named x1,x2,...
    */
  def stringSHwithHarrshNamingtoSH(ssh: StringSymbolicHeap) : (SymbolicHeap, Seq[String], Seq[String]) = {
    val (freeVarsUnsorted,boundVarsUnsorted) = ssh.getVars.toSeq.partition(isHarrshFreeVariableString)
    val (freeVars,boundVars) = (freeVarsUnsorted.sorted, boundVarsUnsorted.sorted)
    val freeVarIds : Seq[Int] = freeVars.map(_.drop(FreeVarString.length)).map(Integer.parseInt) :+ 0
    val numFV = freeVarIds.max
    val filledFreeVars : Seq[Var] = if (freeVars.isEmpty) Seq.empty else mkAllVars(1 to numFV)
    val filledFreeVarString = filledFreeVars map (_.toString)

    val naming : VarUnNaming = seplog.mkUnNaming(filledFreeVarString,boundVars) //mkUnNamingFromIncompleteDefaultNames(freeVars, boundVars)
    val renamedHeap = ssh.replaceStringsByIds(naming)
    (renamedHeap, filledFreeVarString, boundVars)
  }

}