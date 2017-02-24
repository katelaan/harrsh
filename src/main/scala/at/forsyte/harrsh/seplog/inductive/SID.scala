package at.forsyte.harrsh.seplog.inductive

import java.util.NoSuchElementException

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.{PtrExpr, PtrVar, Var}
import at.forsyte.harrsh.util.Combinators

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, rules : Set[Rule], description : String = "Unnamed SID") {

  override def toString = {
    description + " (start predicate '" + startPred + "'): " + rules.toSeq.sortBy(_.head).mkString("\n    ", "\n    ", "")
  }

  // Note that we take the maximum here, because we allow that some of the rules do not mention all FVs (and in particular not the maxFV; see also DefaultSID parser
  lazy val arityOfStartPred : Int = rules.filter(_.head == startPred).map(rule => rule.freeVars.size).max

}

object SID extends HarrshLogging {

  def apply(startPred : String, description : String, rules : (String, Seq[String], SymbolicHeap)*) = new SID(startPred, Set()++(rules map Rule.fromTuple), description)

  def unfold(sid : SID, depth: Int, reducedOnly : Boolean = false): Seq[SymbolicHeap] = {

    logger.debug("Unfolding sid " + sid)

    //def extractBodies(group : (String,Set[Rule])) = (group._1,group._2 map (_.body))
    def extractBodies(group : (String,Set[Rule])) = {
      (group._1,group._2 map (_.body))
    }
    val predsToBodies : Map[String, Set[SymbolicHeap]] = Map() ++ sid.rules.groupBy(_.head).map(extractBodies _)

    val initialArgs : Seq[PtrExpr] = (1 to sid.arityOfStartPred) map (i => PtrVar(Var.mkVar(i)).asInstanceOf[PtrExpr])
    val initial = SymbolicHeap(Seq(PredCall(sid.startPred, initialArgs)))

    logger.debug("Will unfold using the following rules: ")
    for ((k,vs) <- predsToBodies) {
      logger.debug("Pred " + k + ":")
      for (v <- vs) {
        logger.debug(" * " + v)
      }
    }

    val unfolded = try {
      unfoldStep(predsToBodies, Seq(), Seq(initial), depth)
    } catch {
      case e : NoSuchElementException =>
        println("Aborting. The SID appears to contain undefined predicates: " + e.getMessage)
        Seq()
    }

    if (reducedOnly) unfolded.filter(_.predCalls.isEmpty) else unfolded
  }

  private def unfoldStep(predsToBodies: Map[String, Set[SymbolicHeap]], acc : Seq[SymbolicHeap], curr: Seq[SymbolicHeap], depth: Int): Seq[SymbolicHeap] = {
    logger.debug("Currently active instances:" + curr.mkString(", "))
    if (depth == 0) acc ++ curr
    else {
      val allNewInstances = for {
        h <- curr
        if !h.predCalls.isEmpty
        callReplacements = h.predCalls.map(_.name) map predsToBodies
        replacementChoices: Seq[Seq[SymbolicHeap]] = Combinators.choices(callReplacements)
        newInstances: Seq[SymbolicHeap] = replacementChoices.map(h.instantiateCalls(_))
      } yield newInstances

      unfoldStep(predsToBodies, acc ++ curr, allNewInstances.flatten, depth - 1)
    }
  }

  def toHarrshFormat(sid : SID) : Seq[String] = {
    val (start, rest) = sid.rules.partition(_.head == sid.startPred)
    val rulesWithStartFirst : Seq[Rule] = start.toSeq ++ rest
    val undelimitedLines = for {
      rule <- rulesWithStartFirst
      sh = rule.body
    } yield rule.head + " <= " + SymbolicHeap.toHarrshFormat(sh, DefaultNaming)
    undelimitedLines.init.map(_ + " ;") :+ undelimitedLines.last
  }

}