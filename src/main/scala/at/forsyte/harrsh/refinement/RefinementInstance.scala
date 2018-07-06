package at.forsyte.harrsh.refinement

import java.text.SimpleDateFormat

import RefinementInstance._
import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{PtrExpr, Var}
import at.forsyte.harrsh.seplog.inductive.{PredCall, Rule, SID, SymbolicHeap}

import scala.annotation.tailrec

/**
  *
  * @param sid SID to refine
  * @param ha Automaton used in refinement
  * @param topLevelQuery Optional top-level query; if given, will try to find final state for this formula, instead of the SID start predicate
  */
case class RefinementInstance(sid: SID,
                              ha: HeapAutomaton,
                              topLevelQuery: Option[SymbolicHeap] = None,
                              mode: RefinementMode = OnTheFly,
                              reportProgress: Boolean = false) extends HarrshLogging {

  val pred: String = sid.startPred

  /**
    * Mapping from src, label and head predicate to target state for reconstructing the full assignment
    */
  private var combinationsToTargets : Map[(Seq[ha.State],SymbolicHeap,String), Set[ha.State]] = Map.empty
  private var reachedFinalStates : Set[ha.State] = Set.empty

  case class TransitionInstance(srcStates: Seq[ha.State], body: SymbolicHeap, headPredicate: String, headState: ha.State)
  case class RefinementState(empty: Boolean, reached: Set[TransitionInstance]) {

    /**
      * Convert refinement state to SID
      * @return The refined SID as well as a flag indicating whether it is empty
      */
    def toSID : (SID,Boolean) = {
      // Assign suffixes to each state
      val states : Set[ha.State] = (for (TransitionInstance(states, _, _, headState) <- reached) yield states :+ headState).flatten
      val stateToIndex : Map[ha.State, Int] = states.toSeq.zipWithIndex.toMap

      val innerRules = for {
        TransitionInstance(states,body,head,headState) <- reached.toSeq
      } yield Rule(
        head = head+stateToIndex(headState),
        freeVars = body.freeVars map (_.toString),
        qvars = body.boundVars.toSeq map (_.toString),
        body = SymbolicHeap.addTagsToPredCalls(body, states map (s => ""+stateToIndex(s))))
      val finalRules = reachedFinalStates.toSeq.map{
        state =>
          val call = PredCall(sid.startPred+stateToIndex(state), (1 to sid.arityOfStartPred) map (i => PtrExpr(Var(i))))
          Rule(
            head = sid.startPred,
            freeVars = (1 to sid.arityOfStartPred) map (Var(_).toString),
            qvars = Seq(),
            body = SymbolicHeap(Seq.empty, Seq(call))
          )
      }

      if (reachedFinalStates.isEmpty) {
        logger.info("Refined SID is empty")
      }

      (SID(
        startPred = sid.startPred,
        rules = innerRules ++ finalRules,
        description = "Refinement of " + sid.description + " with " + ha.description,
        numFV = sid.numFV
      ), reachedFinalStates.isEmpty)
    }

  }

  /**
    * @return True iff there is no RSH in the refinement of sid by ha
    */
  def run : RefinementState = {
    computeRefinementFixedPoint(Set(), Set(), 1)
  }

  @tailrec
  private def computeRefinementFixedPoint(r : Set[(String, ha.State)], previousCombinations : Set[(Seq[ha.State],SymbolicHeap,String)], iteration : Int) : RefinementState = {
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

    def performSingleIteration: Seq[((String, ha.State), (Seq[ha.State],SymbolicHeap,String))] = {
      if (ha.implementsTargetComputation) {
        for {
          Rule(head, _, _, body) <- sid.rules
          src <- {
            val srcs  = allDefinedSources(r, body.identsOfCalledPreds)
            logger.debug("Looking at defined sources for " + head + " <= " + body + "; found " + srcs.size)
            srcs
          }
          // Only go on if we haven't tried this combination in a previous iteration
          if {
            logger.debug("Computing targets for " + head + " <= " + body + " from source " + src + " ?")
            !previousCombinations.contains((src, body, head))
          }
          trg <- {
            logger.debug("Yes, targets not computed previously, get targets for " + body)
            ha.getTargetsFor(src, body)
          }
        } yield ((head, trg), (src,body,head))
      } else {
        // No dedicated target computation, need to brute-force
        for {
          Rule(head, _, _, body) <- sid.rules
          src <- allDefinedSources(r, body.identsOfCalledPreds)
          // Only go on if we haven't tried this combination in a previous iteration
          if !previousCombinations.contains((src, body, head))
          // No smart target computation, have to iterate over all possible targets
          trg <- ha.states
          if ha.isTransitionDefined(src, trg, body)
        } yield ((head, trg), (src,body,head))
      }
    }

    def isFinal(pair : (String,ha.State)) : Boolean = pair._1 == pred && ha.isFinal(pair._2)

    if (mode == FullRefinement && iteration == 1) {
      // Reset state
      combinationsToTargets = Map.empty
      reachedFinalStates = Set.empty
    }

    val discoveredStartPredicate = r.find(isFinal)
    if (discoveredStartPredicate.isDefined && mode == FullRefinement) {
      // Save reached final states for generation of refined SID
      reachedFinalStates = reachedFinalStates ++ r.filter(isFinal).map(_._2)
    }

    if (discoveredStartPredicate.isDefined && mode == OnTheFly) {
      // There is a derivation that reaches a final state, refined language nonempty
      // We only continue the fixed-point computation if we're interested in the full refinement; otherwise we return false
      logger.debug("Reached " + discoveredStartPredicate.get + " => language is non-empty")
      RefinementState(empty = false, Set.empty)
    } else {
      logger.debug("Beginning iteration #" + iteration)
      val iterationResult = performSingleIteration
      val (newPairs, newCombs) = iterationResult.unzip
      val union = r ++ newPairs

      if (mode == FullRefinement) {
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
      if (reportProgress) println(dateFormat.format(new java.util.Date) + " -- Refinement iteration: #" + iteration + " Discovered " + newPairs.size + " targets; total w/o duplicates: " + union.size)

      if (union.size == r.size) {
        // Fixed point reached without reaching a pred--final-state pair
        logger.debug("Fixed point: " + union.mkString(", "))
        if (discoveredStartPredicate.isEmpty) {
          logger.debug("=> Language is empty")
        }
        // Only compute the new combinations + mapping to targets if desired (i.e., if full refinement was asked for)
        // (to save some computation time in the cases where we're only interested in a yes/no-answer)
        val newCombinations: Set[TransitionInstance] = if (mode == FullRefinement) {
          for {
            comb <- previousCombinations ++ newCombs
            trg <- combinationsToTargets(comb)
          } yield TransitionInstance(comb._1,comb._2,comb._3,trg)
        } else {
          Set.empty
        }

        RefinementState(empty = true, newCombinations)
      } else {
        // Fixed point not yet reached, recurse
        val unionOfPrevs = previousCombinations ++ newCombs
        computeRefinementFixedPoint(union, unionOfPrevs, iteration + 1)
      }
    }
  }

  private lazy val dateFormat : SimpleDateFormat = new SimpleDateFormat("hh:mm:ss.SSS")

}

object RefinementInstance {

  type RefinementMode = Boolean
  val OnTheFly = false
  val FullRefinement = true

}
