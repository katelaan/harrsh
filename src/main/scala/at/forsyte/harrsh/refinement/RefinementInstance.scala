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

  type TransitionTargetCombination = (Seq[ha.State], SymbolicHeap, String)
  type IterationResult = Seq[((String, ha.State), TransitionTargetCombination)]

  case class ReachedStates(pairs: Set[(String, ha.State)], cachedFinalStates: Option[Set[ha.State]] = None) {
    // TODO: These are just the final states for the start predicate. Need to modify this when we pass a top-level query
    val finalStates : Set[ha.State] = cachedFinalStates.getOrElse{
      ReachedStates.finalStates(pairs)
    }

    def reachedStatesForPred(pred : String) : Set[ha.State] = pairs filter (_._1 == pred) map (_._2)

    def containsFinalState: Boolean = finalStates.nonEmpty

    def size: Int = pairs.size

    def ++(otherPairs: Iterable[(String, ha.State)]): ReachedStates = {
      ReachedStates(pairs ++ otherPairs, Some(finalStates ++ ReachedStates.finalStates(otherPairs)))
    }
  }
  object ReachedStates {
    def empty = ReachedStates(Set.empty)
    def isFinal(pair : (String,ha.State)) : Boolean = pair._1 == pred && ha.isFinal(pair._2)
    def finalStates(pairs: Iterable[(String, ha.State)]): Set[ha.State] = pairs.filter(isFinal).map(_._2).toSet
  }

  case class TransitionInstance(srcStates: Seq[ha.State], body: SymbolicHeap, headPredicate: String, headState: ha.State)

  /**
    * Mapping from src, label and head predicate to target state for reconstructing the full assignment
    * (Only used in computation of full refinement, not in on-the-fly refinement.)
    */
  case class TransitionsToTrgStateMap(map: Map[TransitionTargetCombination, Set[ha.State]] = Map.empty) {

    def apply(k: TransitionTargetCombination) = map(k)

    def extendWith(kvPairs: Seq[((Seq[ha.State], SymbolicHeap, String), ha.State)]): TransitionsToTrgStateMap = {
      // TODO: Stateless solution
      var newMap = map
      for ((k,v) <- kvPairs) {
        if (newMap.isDefinedAt(k)) {
          newMap = newMap + (k -> (newMap(k) + v))
        } else {
          newMap = newMap + (k -> Set(v))
        }
      }
      TransitionsToTrgStateMap(newMap)
    }
  }

  case class RefinementState(empty: Boolean,
                             reachedStates: ReachedStates,
                             reachedTransitions: Set[TransitionInstance]) {

    /**
      * Convert refinement state to SID
      * @return The refined SID as well as a flag indicating whether it is empty
      */
    def toSID : (SID,Boolean) = {
      // Assign suffixes to each state
      val states : Set[ha.State] = (for (TransitionInstance(states, _, _, headState) <- reachedTransitions) yield states :+ headState).flatten
      val stateToIndex : Map[ha.State, Int] = states.toSeq.zipWithIndex.toMap

      val innerRules = for {
        TransitionInstance(states,body,head,headState) <- reachedTransitions.toSeq
      } yield Rule(
        head = head+stateToIndex(headState),
        freeVars = body.freeVars map (_.toString),
        qvars = body.boundVars.toSeq map (_.toString),
        body = SymbolicHeap.addTagsToPredCalls(body, states map (s => ""+stateToIndex(s))))
      val finalRules = reachedStates.finalStates.toSeq.map{
        state =>
          val call = PredCall(sid.startPred+stateToIndex(state), (1 to sid.arityOfStartPred) map (i => PtrExpr(Var(i))))
          Rule(
            head = sid.startPred,
            freeVars = (1 to sid.arityOfStartPred) map (Var(_).toString),
            qvars = Seq(),
            body = SymbolicHeap(Seq.empty, Seq(call))
          )
      }

      if (reachedStates.finalStates.isEmpty) {
        logger.info("Refined SID is empty")
      }

      (SID(
        startPred = sid.startPred,
        rules = innerRules ++ finalRules,
        description = "Refinement of " + sid.description + " with " + ha.description,
        numFV = sid.numFV
      ), !reachedStates.containsFinalState)
    }

  }

  /**
    * @return True iff there is no RSH in the refinement of sid by ha
    */
  def run : RefinementState = {
    computeRefinementFixedPoint(ReachedStates.empty, Set(), TransitionsToTrgStateMap(), iteration = 1)
  }

  private def allDefinedSources(reached : ReachedStates, calls : Seq[String]) : Set[Seq[ha.State]] = {
    if (calls.isEmpty) {
      Set(Seq())
    } else {
      for {
        tail <- allDefinedSources(reached, calls.tail)
        head <- reached.reachedStatesForPred(calls.head)
      } yield head +: tail
    }
  }

  private def performSingleIteration(reached : ReachedStates,
                                     previousCombinations : Set[TransitionTargetCombination]): IterationResult = {
    if (ha.implementsTargetComputation) {
      for {
        Rule(head, _, _, body) <- sid.rules
        src <- {
          val srcs  = allDefinedSources(reached, body.identsOfCalledPreds)
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
        src <- allDefinedSources(reached, body.identsOfCalledPreds)
        // Only go on if we haven't tried this combination in a previous iteration
        if !previousCombinations.contains((src, body, head))
        // No smart target computation, have to iterate over all possible targets
        trg <- ha.states
        if ha.isTransitionDefined(src, trg, body)
      } yield ((head, trg), (src,body,head))
    }
  }

  private def extendTransitionsToTrgMap(combinationsToTargets: TransitionsToTrgStateMap, iterationResult: IterationResult): TransitionsToTrgStateMap = {
    if (mode == FullRefinement) {
      // In the computation of the full refinement, we must remember the targets of each of the combinations we tried
      val kvPairs = iterationResult map {
        case ((_,trg),comb) => (comb,trg)
      }
      combinationsToTargets.extendWith(kvPairs)
    } else {
      combinationsToTargets
    }
  }

  private def toTransitionInstances(previousCombinations: Set[TransitionTargetCombination],
                                    newCombinations: Seq[TransitionTargetCombination],
                                    transitionsToTrgStateMap: TransitionsToTrgStateMap) : Set[TransitionInstance] = {
    if (mode == FullRefinement) {
      // Only compute the new combinations + mapping to targets if desired (i.e., if full refinement was asked for)
      // (to save some computation time in the cases where we're only interested in a yes/no-answer)
      for {
        comb <- previousCombinations ++ newCombinations
        trg <- transitionsToTrgStateMap(comb)
      } yield TransitionInstance(comb._1,comb._2,comb._3,trg)
    } else {
      Set.empty
    }
  }

  @tailrec
  private def computeRefinementFixedPoint(reached : ReachedStates,
                                          previousCombinations : Set[TransitionTargetCombination],
                                          transitionsToTrgStates: TransitionsToTrgStateMap,
                                          iteration : Int) : RefinementState = {

    if (reached.containsFinalState && mode == OnTheFly) {
      // There is a derivation that reaches a final state, refined language nonempty
      // We only continue the fixed-point computation if we're interested in the full refinement; otherwise we return false
      logger.debug("Reached " + reached.finalStates.head + " => language is non-empty")
      RefinementState(empty = false, reached, Set.empty)
    } else {
      logger.debug("Beginning iteration #" + iteration)
      val iterationResult = performSingleIteration(reached, previousCombinations)
      val (newPairs, newCombs) = iterationResult.unzip
      val union = reached ++ newPairs
      val updatedTransitionsToTrgStates = extendTransitionsToTrgMap(transitionsToTrgStates, iterationResult)

      logger.debug("Refinement iteration: #" + iteration + " " + (if (newPairs.isEmpty) "--" else newPairs.mkString(", ")))
      if (reportProgress) println(dateFormat.format(new java.util.Date) + " -- Refinement iteration: #" + iteration + " Discovered " + newPairs.size + " targets; total w/o duplicates: " + union.size)

      if (union.size == reached.size) {
        // Fixed point reached
        logger.debug("Fixed point: " + union.pairs.mkString(", "))
        if (!reached.containsFinalState) {
          logger.debug("=> Language is empty")
        }
        val transitions = toTransitionInstances(previousCombinations, newCombs, transitionsToTrgStates)

        RefinementState(empty = true, reached, transitions)
      } else {
        // Fixed point not yet reached, recurse
        val unionOfPrevs = previousCombinations ++ newCombs
        computeRefinementFixedPoint(union, unionOfPrevs, updatedTransitionsToTrgStates, iteration + 1)
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
