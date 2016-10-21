package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.main.SlexLogging
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

import scala.annotation.tailrec


/**
  * Created by jens on 10/15/16.
  */
object RefinementAlgorithms extends SlexLogging {

  def refineSID(sid : SID, ha : HeapAutomaton) : SID = {
    // TODO: Implement
    ???
  }

  /**
    * @return True iff there is no RSH in the refinement of sid by ha
    */
  def onTheFlyEmptinessCheck(sid : SID, ha : HeapAutomaton) : Boolean = {
    computeRefinementFixedPoint(sid, sid.startPred, ha)(Set(), Set(), 1)
  }

  @tailrec
  private def computeRefinementFixedPoint(sid : SID, pred : String, ha : HeapAutomaton)(r : Set[(String, ha.State)], previousCombinations : Set[(Seq[ha.State],SymbolicHeap,String)], iteration : Int) : Boolean = {

    def reachedStatesForPred(rel : Set[(String, ha.State)], call : String) : Set[ha.State] = rel filter (_._1 == call) map (_._2)

    def allDefinedSources(rel : Set[(String, ha.State)], calls : Seq[String]) : Set[Seq[ha.State]] = {
      if (calls.isEmpty) {
        Set(Seq())
      } else {
        for {
          tail <- allDefinedSources(rel, calls.tail)
          head <- reachedStatesForPred(rel, calls.head)
        } yield head +: tail
      }
    }

    def performSingleIteration: Set[((String, ha.State), (Seq[ha.State],SymbolicHeap,String))] = {
      if (ha.implementsTargetComputation) {
        for {
          (head, body) <- sid.rules
          src <- allDefinedSources(r, body.calledPreds)
          // Only go on if we haven't tried this combination in a previous iteration
          if !previousCombinations.contains((src, body, head))
          trg <- ha.getTargetsFor(src, body)
        } yield ((head, trg), (src,body,head))
      } else {
        // No dedicated target computation, need to brute-force
        for {
          (head, body) <- sid.rules
          src <- allDefinedSources(r, body.calledPreds)
          // Only go on if we haven't tried this combination in a previous iteration
          if !previousCombinations.contains((src, body, head))
          // No smart target computation, have to iterate over all possible targets
          trg <- ha.states
          if ha.isTransitionDefined(src, trg, body)
        } yield ((head, trg), (src,body,head))
      }
    }

    val discoveredStartPredicate = r.find(p => p._1 == pred && ha.isFinal(p._2))

    if (discoveredStartPredicate.isDefined) {
      // There is a derivation that reaches a final state, refined language nonempty
      logger.debug("Reached " + discoveredStartPredicate.get + " => language is non-empty")
      false
    } else {
      val (newPairs, newCombs) = performSingleIteration.unzip

      logger.debug("Iteration: #" + iteration + " " + (if (newPairs.isEmpty) "--" else newPairs.mkString(", ")))

      val union = r union newPairs
      if (union.size == r.size) {
        // Fixed point reached without reaching a pred--final-state pair
        logger.debug("Fixed point: " + union.mkString(", "))
        logger.debug("=> Language is empty")
        true
      } else {
        // Fixed point not yet reached, recurse
        val unionOfPrevs = previousCombinations union newCombs
        computeRefinementFixedPoint(sid, pred, ha)(union, unionOfPrevs, iteration + 1)
      }
    }
  }


}
