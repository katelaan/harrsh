package at.forsyte.harrsh.seplog.inductive

import java.util.NoSuchElementException

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jens on 3/6/17.
  */
object SIDUnfolding extends HarrshLogging {

  def unfoldSingleCall(sh : SymbolicHeap, call : PredCall, sid : SID) : Seq[SymbolicHeap] = {
    logger.debug("Unfolding " + call + " in " + sh)

    (for (body <- sid.rulesAsHeadToBodyMap(call.name)) yield sh.instantiateCall(call, body)).toSeq
  }

  def unfold(sid : SID, depth: Int, reducedOnly : Boolean = false): Seq[SymbolicHeap] = {

    logger.debug("Unfolding sid " + sid)

    val predsToBodies: Map[String, Set[SymbolicHeap]] = sid.rulesAsHeadToBodyMap

    val initial: SymbolicHeap = sid.callToStartPred

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

  /**
    * Unfold all given heaps exactly once, returning both reduced and non-reduced results
    * @param sid Underlying SID
    * @param heaps Heaps to unfold
    * @return Unfolded heaps
    */
  def unfoldOnce(sid : SID, heaps : Seq[SymbolicHeap]) : Seq[SymbolicHeap] = unfoldStep(sid.rulesAsHeadToBodyMap, Seq.empty, heaps, 1)

  private def unfoldStep(predsToBodies: Map[String, Set[SymbolicHeap]], acc : Seq[SymbolicHeap], curr: Seq[SymbolicHeap], depth: Int): Seq[SymbolicHeap] = {
    logger.debug("Currently active instances:" + curr.mkString(", "))
    if (depth == 0) acc ++ curr
    else {
      val allNewInstances = for {
        sh <- curr
        if !sh.predCalls.isEmpty
        callReplacements = sh.predCalls.map(_.name) map predsToBodies
        replacementChoices: Seq[Seq[SymbolicHeap]] = Combinators.choices(callReplacements)
        newInstances: Seq[SymbolicHeap] = replacementChoices.map(sh.instantiateCalls(_))
      } yield newInstances

      unfoldStep(predsToBodies, acc ++ curr, allNewInstances.flatten, depth - 1)
    }
  }

  def firstReducedUnfolding(sid : SID) : SymbolicHeap = {

    // TODO This is an extremely inefficient way to implement this functionality; we should at least short circuit the unfold process upon finding an RSH, or better, implement the obvious linear time algorithm for generating the minimal unfolding
    def unfoldAndGetFirst(depth : Int) : SymbolicHeap = unfold(sid, depth, true).headOption match {
      case None => unfoldAndGetFirst(depth+1)
      case Some(sh) => sh
    }

    unfoldAndGetFirst(1)
  }

}
