package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.{DefaultNaming, PtrExpr, PtrVar, Var}

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, rules : Seq[Rule], description : String = "Unnamed SID", numFV : Int) {

  override def toString = {
    description + " (start predicate '" + startPred + "'): " + rules.toSeq.sortBy(_.head).mkString("\n    ", "\n    ", "")
  }

  // Note that we take the maximum here, because we allow that some of the rules do not mention all FVs (and in particular not the maxFV; see also DefaultSID parser
  lazy val arityOfStartPred : Int = rules.filter(_.head == startPred).map(rule => rule.freeVars.size).max

  lazy val maximumArity : Int = rules.map(rule => rule.freeVars.size).max

  def callToStartPred: SymbolicHeap = {
    val initialArgs: Seq[PtrExpr] = (1 to arityOfStartPred) map (i => PtrVar(Var.mkVar(i)).asInstanceOf[PtrExpr])
    val initial = SymbolicHeap(Seq.empty, Seq(PredCall(startPred, initialArgs)))
    initial
  }

  lazy val rulesAsHeadToBodyMap: Map[String, Set[SymbolicHeap]] = {
    def extractBodies(group: (String, Seq[Rule])) = {
      (group._1, group._2 map (_.body) toSet)
    }
    Map() ++ rules.groupBy(_.head).map(extractBodies)
  }

  def toHarrshFormat : Seq[String] = {
    val (start, rest) = rules.partition(_.head == startPred)
    val rulesWithStartFirst : Seq[Rule] = start.toSeq ++ rest
    val undelimitedLines = for {
      rule <- rulesWithStartFirst
      sh = rule.body
    } yield rule.head + " <= " + SymbolicHeap.toHarrshFormat(sh, DefaultNaming)
    undelimitedLines.init.map(_ + " ;") :+ undelimitedLines.last
  }

}

object SID extends HarrshLogging {

  def apply(startPred : String, description : String, rules : (String, Seq[String], SymbolicHeap)*) = new SID(startPred, rules map Rule.fromTuple, description, rules.map(_._3.numFV).max)

}