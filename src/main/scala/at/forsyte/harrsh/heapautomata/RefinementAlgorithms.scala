package at.forsyte.harrsh.heapautomata

import java.text.SimpleDateFormat

import at.forsyte.harrsh.main.SlexLogging
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID, SymbolicHeap}

import scala.annotation.tailrec


/**
  * Created by jens on 10/15/16.
  */
class RefinementAlgorithms(sid : SID, ha : HeapAutomaton) extends SlexLogging {

  def refineSID(reportProgress : Boolean) : SID = {

    val (empty, reach) = computeRefinementFixedPoint(sid.startPred, computeFullRefinement = true, reportProgress = reportProgress)(Set(), Set(), 1)

    // Assign suffixes to each state
    val states : Set[ha.State] = (for ((states, _, _, headState) <- reach) yield states :+ headState).flatten
    val stateToIndex : Map[ha.State, Int] = Map() ++ states.toSeq.zipWithIndex

    val innerRules = for {
        (states,body,head,headState) <- reach
      } yield (head+stateToIndex(headState), body.addToCallPreds(states map (s => ""+stateToIndex(s))))
    val finalRules = reachedFinalStates.map(state => (sid.startPred, SymbolicHeap(Seq(PredCall(sid.startPred+stateToIndex(state), (1 to sid.arityOfStartPred) map fv)))))

    if (reachedFinalStates.isEmpty) {
      logger.info("Refined SID is empty")
      println("WARNING: Language of refined SID is empty (no rules for start predicate '" + sid.startPred + "').")
    }

    SID(
      startPred = sid.startPred,
      rules = innerRules ++ finalRules,
      description = "Refinement of " + sid.description + " with " + ha.description
    )

  }

  /**
    * @return True iff there is no RSH in the refinement of sid by ha
    */
  def onTheFlyEmptinessCheck(reportProgress : Boolean) : Boolean = {
    computeRefinementFixedPoint(sid.startPred, computeFullRefinement = false, reportProgress = reportProgress)(Set(), Set(), 1)._1
  }

  /**
    * Mapping from src, label and head predicate to target state for reconstructing the full assignment
    */
  private var combinationsToTargets : Map[(Seq[ha.State],SymbolicHeap,String), Set[ha.State]] = Map.empty
  private var reachedFinalStates : Set[ha.State] = Set.empty

  @tailrec
  private def computeRefinementFixedPoint(pred : String, computeFullRefinement : Boolean, reportProgress : Boolean)(r : Set[(String, ha.State)], previousCombinations : Set[(Seq[ha.State],SymbolicHeap,String)], iteration : Int) : (Boolean,Set[(Seq[ha.State],SymbolicHeap,String,ha.State)]) = {
    // TODO The refinment fixed point computation is quite long and convoluted now. Cleanup

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

    def isFinal(pair : (String,ha.State)) : Boolean = pair._1 == pred && ha.isFinal(pair._2)

    if (computeFullRefinement && iteration == 1) {
      // Reset state
      combinationsToTargets = Map.empty
      reachedFinalStates = Set.empty
    }

    val discoveredStartPredicate = r.find(isFinal)
    if (discoveredStartPredicate.isDefined && computeFullRefinement) {
      // Save reached final states for generation of refined SID
      reachedFinalStates = reachedFinalStates ++ r.filter(isFinal).map(_._2)
    }

    if (discoveredStartPredicate.isDefined && !computeFullRefinement) {
      // There is a derivation that reaches a final state, refined language nonempty
      // We only continue the fixed-point computation if we're interested in the full refinement; otherwise we return false
      logger.debug("Reached " + discoveredStartPredicate.get + " => language is non-empty")
      (false, Set.empty)
    } else {
      val iterationResult = performSingleIteration
      val (newPairs, newCombs) = iterationResult.unzip
      val union = r union newPairs

      if (computeFullRefinement) {
        // In the computation of the full refinement, we must remember the targets of each of the combinations we tried
        val kvPairs = iterationResult map {
          case ((_,trg),comb) => (comb,trg)
        }
        for ((k,v) <- kvPairs) {
          if (combinationsToTargets.isDefinedAt(k)) {
            combinationsToTargets = combinationsToTargets + (k -> (combinationsToTargets(k) + v))
          } else {
            combinationsToTargets = combinationsToTargets + (k -> Set(v))
          }
        }
      }

      logger.debug("Refinement iteration: #" + iteration + " " + (if (newPairs.isEmpty) "--" else newPairs.mkString(", ")))
      if (reportProgress) println(dateFormat.format(new java.util.Date()) + " -- Refinement iteration: #" + iteration + " Discovered " + newPairs.size + " targets; total w/o duplicates: " + union.size)

      if (union.size == r.size) {
        // Fixed point reached without reaching a pred--final-state pair
        logger.debug("Fixed point: " + union.mkString(", "))
        logger.debug("=> Language is empty")
        // Only compute the new combinations + mapping to targets if desired (i.e., if full refinement was asked for)
        // (to save some computation time in the cases where we're only interested in a yes/no-answer)
        (true, if (computeFullRefinement) (previousCombinations union newCombs).flatMap(t => combinationsToTargets(t) map (trg => (t._1,t._2,t._3,trg))) else Set.empty)
      } else {
        // Fixed point not yet reached, recurse
        val unionOfPrevs = previousCombinations union newCombs
        computeRefinementFixedPoint(pred, computeFullRefinement, reportProgress)(union, unionOfPrevs, iteration + 1)
      }
    }
  }

  private lazy val dateFormat : SimpleDateFormat = new SimpleDateFormat("hh:mm:ss.SSS");

}
