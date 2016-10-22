package at.forsyte.harrsh.heapautomata

import java.io.FileNotFoundException
import java.text.SimpleDateFormat

import at.forsyte.harrsh.main.{Benchmarking, SlexLogging, TaskConfig}
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Created by jens on 10/15/16.
  */
object RefinementAlgorithms extends SlexLogging {

  def refineSID(file : String, property : AutomatonTask, timeout : Duration, reportProgress : Boolean) : Option[SID] = {

    val task = TaskConfig(file, property, None)
    try {
      val (sid, ha) = Benchmarking.prepareBenchmark(task)

      val f: Future[SID] = Future {
        refineSID(sid, ha, reportProgress = reportProgress)
      }

      try {
        val sid = Await.result(f, timeout)
        Some(sid)
      } catch {
        case e : TimeoutException =>
          println("reached timeout (" + timeout + ")")
          None
      }

    } catch {
      case e : FileNotFoundException =>
        println("Could not open file " + file)
        None
    }
  }

  def refineSID(sid : SID, ha : HeapAutomaton, reportProgress : Boolean) : SID = {

    val (empty, reach) = computeRefinementFixedPoint(computeFullRefinement = true, reportProgress, sid, sid.startPred, ha)(Set(), Set(), 1)

    SID(
      startPred = sid.startPred,
      rules = for {
        (states,body,head) <- reach
      } yield (head, body.tagCallsWith(states map (s => ""+s.hashCode()))),
      description = "Refinement of " + sid.description + " with " + ha.description
    )

  }

  /**
    * @return True iff there is no RSH in the refinement of sid by ha
    */
  def onTheFlyEmptinessCheck(sid : SID, ha : HeapAutomaton, reportProgress : Boolean) : Boolean = {
    computeRefinementFixedPoint(computeFullRefinement = false, reportProgress, sid, sid.startPred, ha)(Set(), Set(), 1)._1
  }

  @tailrec
  private def computeRefinementFixedPoint(computeFullRefinement : Boolean, reportProgress : Boolean, sid : SID, pred : String, ha : HeapAutomaton)(r : Set[(String, ha.State)], previousCombinations : Set[(Seq[ha.State],SymbolicHeap,String)], iteration : Int) : (Boolean,Set[(Seq[ha.State],SymbolicHeap,String)]) = {

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

    if (discoveredStartPredicate.isDefined && !computeFullRefinement) {
      // There is a derivation that reaches a final state, refined language nonempty
      // We only continue the fixed-point computation if we're interested in the full refinement; otherwise we return false
      logger.debug("Reached " + discoveredStartPredicate.get + " => language is non-empty")
      (false, Set.empty)
    } else {
      val (newPairs, newCombs) = performSingleIteration.unzip
      val union = r union newPairs

      logger.debug("Refinement iteration: #" + iteration + " " + (if (newPairs.isEmpty) "--" else newPairs.mkString(", ")))
      if (reportProgress) println(dateFormat.format(new java.util.Date()) + " -- Refinement iteration: #" + iteration + " Discovered " + newPairs.size + " targets; total w/o duplicates: " + union.size)

      if (union.size == r.size) {
        // Fixed point reached without reaching a pred--final-state pair
        logger.debug("Fixed point: " + union.mkString(", "))
        logger.debug("=> Language is empty")
        // Only compute the new combinations if desired
        // (to save some computation time in the cases where we're only interested in a yes/no-answer)
        (true, if (computeFullRefinement) (previousCombinations union newCombs) else Set.empty)
      } else {
        // Fixed point not yet reached, recurse
        val unionOfPrevs = previousCombinations union newCombs
        computeRefinementFixedPoint(computeFullRefinement, reportProgress, sid, pred, ha)(union, unionOfPrevs, iteration + 1)
      }
    }
  }

  private lazy val dateFormat : SimpleDateFormat = new SimpleDateFormat("hh:mm:ss.SSS");

}
