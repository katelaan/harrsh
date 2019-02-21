package at.forsyte.harrsh.refinement

import java.text.SimpleDateFormat

import RefinementInstance._
import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._

import scala.annotation.tailrec

/**
  *
  * @param sid SID to refine
  * @param ha Automaton used in refinement
  * @param topLevelQuery Optional top-level query; if given, will try to find final state for this formula, instead of the SID start predicate
  */
case class RefinementInstance(sid: SidLike,
                              ha: HeapAutomaton,
                              topLevelQuery: Option[SymbolicHeap],
                              mode: RefinementMode,
                              incrementalFromNumCalls: Option[Int],
                              skipSinksAsSources: Boolean,
                              reportProgress: Boolean) extends HarrshLogging {

  // TODO: Take top level query into account?
  assert(topLevelQuery.isEmpty)

  val incrementalBound: Int = incrementalFromNumCalls.getOrElse(DefaultIncrementalFromNumCalls)

  val pred: String = sid.startPred
  logger.debug("Will run refinement with goal predicate " + pred)

  type TransitionTargetCombination = (Seq[ha.State], SymbolicHeap, String)
  type IterationResult = Seq[((String, ha.State), TransitionTargetCombination)]

  case class ReachedStates(pairs: Set[(String, ha.State)], cachedFinalStates: Option[Set[ha.State]] = None) {
    // TODO: These are just the final states for the start predicate. Need to modify this when we pass a top-level query
    val finalStates : Set[ha.State] = cachedFinalStates.getOrElse{
      ReachedStates.finalStates(pairs)
    }

    def reachedStatesForPred(pred : String) : Set[ha.State] = {
      pairs filter (_._1 == pred) map (_._2)
    }

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

  case class Transitions(combinations: Set[TransitionTargetCombination], maybeCombsToStates: Option[TransitionsToTrgStateMap] = None) {

    def extend(iterationResult: IterationResult): Transitions = {
      val newCombs = iterationResult map (_._2)
      val unionCombs = combinations ++ newCombs

      val extendedTargets = maybeCombsToStates map { combsToStates =>
        // Remember the targets of each of the combinations we tried
        val kvPairs = iterationResult map {
          case ((_, trg), comb) => (comb, trg)
        }
        combsToStates.extendWith(kvPairs)
      }

      Transitions(unionCombs, extendedTargets)
    }

    def toTransitionInstances : Set[TransitionInstance] = {
      val maybeInstances = maybeCombsToStates map { combsToStates =>
        for {
          comb <- combinations
          trg <- combsToStates(comb)
        } yield TransitionInstance(comb._1, comb._2, comb._3, trg)
      }

      maybeInstances.getOrElse(Set.empty)
    }

  }
  object Transitions {
    def empty: Transitions = Transitions(Set.empty, if (mode == FullRefinement) Some(TransitionsToTrgStateMap()) else None)
  }

  case class RefinementState(empty: Boolean,
                             reachedStates: ReachedStates,
                             reachedTransitions: Set[TransitionInstance])
  {

    /**
      * Convert refinement state to SID
      * @return The refined SID as well as a flag indicating whether it is empty
      */
    def toSID : (SID,Boolean) = {
      // Assign suffixes to each state
      val states : Set[ha.State] = (for (TransitionInstance(states, _, _, headState) <- reachedTransitions) yield states :+ headState).flatten
      val stateToIndex : Map[ha.State, Int] = states.toSeq.zipWithIndex.toMap

      val innerRules = for {
        TransitionInstance(states, body, head, headState) <- reachedTransitions.toSeq
      } yield (head+stateToIndex(headState), RuleBody(
        qvarNames = body.boundVars.toSeq map (_.toString),
        body = SymbolicHeap.addTagsToPredCalls(body, states map (s => ""+stateToIndex(s)))))
      val finalRules = reachedStates.finalStates.toSeq.map{
        state =>
          val freeVars = sid.callToStartPred.freeVars
          val call = PredCall(sid.startPred+stateToIndex(state), freeVars)
          (sid.startPred, RuleBody(
            qvarNames = Seq(),
            body = SymbolicHeap(Seq.empty, Seq.empty, Seq(call), freeVars)
          ))
      }

      if (reachedStates.finalStates.isEmpty) {
        logger.info("Refined SID is empty")
      }

      (SidFactory.makeSidfromRuleBodies(
        sid.startPred,
        innerRules ++ finalRules,
        description = "Refinement of " + sid.description + " with " + ha.description
      ), !reachedStates.containsFinalState)
    }

  }

  /**
    * @return True iff there is no RSH in the refinement of sid by ha
    */
  def run : RefinementState = {
    computeRefinementFixedPoint(ReachedStates.empty, Transitions.empty, iteration = 1)
  }

  private def allDefinedSources(reached : ReachedStates, calls : Seq[String]) : Stream[Seq[ha.State]] = {
    if (calls.isEmpty) {
      Stream(Seq())
    } else {
      for {
        tail <- allDefinedSources(reached, calls.tail)
        head <- reached.reachedStatesForPred(calls.head) filter (s => (!skipSinksAsSources) || ha.isNonSink(s))
      } yield head +: tail
    }
  }

  private def incrementalInstantiation(head: String,
                                       body: SymbolicHeap,
                                       reached : ReachedStates,
                                       break: FlagWrapper,
                                       callsToInstantiate : Seq[String],
                                       picked: Seq[ha.State] = Seq.empty,
                                       target: Option[ha.State] = None) : Stream[(Seq[ha.State],ha.State)] = {
    val isGoal = mode == OnTheFly && head == pred
    if (callsToInstantiate.isEmpty) {
      val trg = target.get
      if (isGoal && ha.isFinal(trg)) {
        break.flag = true
      }
      Stream((picked, target.get))
    } else {
      for {
        next <- (reached.reachedStatesForPred(callsToInstantiate.head) filter (s => (!skipSinksAsSources) || ha.isNonSink(s))).toStream
        if !break.flag
        extended = picked :+ next
        partialTarget <- ha.getPartialTargetsFor(extended, body)
        if ha.isNonSink(partialTarget)
        completed <- incrementalInstantiation(head, body, reached, break, callsToInstantiate.tail, extended, Some(partialTarget))
      } yield completed
    }
  }

  private def considerAllSourceCombinations(head: String,
                                            body: SymbolicHeap,
                                            reached : ReachedStates,
                                            previousTransitions : Transitions,
                                            break: FlagWrapper): Stream[(Seq[ha.State],ha.State)] = {
    val previousCombinations = previousTransitions.combinations
    // In on-the-fly refinement, short-circuit when a final state for the target pred is reached
    val isGoal = mode == OnTheFly && head == pred
    for {
      src <- allDefinedSources(reached, body.identsOfCalledPreds)
      if !break.flag
      // Only go on if we haven't tried this combination in a previous iteration
      if {
        logger.debug("Computing targets for " + head + " <= " + body + " from source " + src + " ?")
        !previousCombinations.contains((src, body, head))
      }
      trg <- {
        logger.debug("Yes, targets not computed previously, get targets for " + body)
        val trg = ha.getTargetsFor(src, body)
        if (isGoal && trg.exists(ha.isFinal)) {
          //println("Found target")
          break.flag = true
        }
        trg
      }
    } yield (src, trg)
  }

  private def shouldTryAllSources(sh: SymbolicHeap) = {
    // We'll try all combinations if the number of pred calls is small
    !ha.implementsPartialTargets || sh.predCalls.size < incrementalBound
  }

  case class FlagWrapper(var flag: Boolean)

  private def performSingleIteration(reached : ReachedStates,
                                     previousTransitions : Transitions): IterationResult = {
    var break = FlagWrapper(false)
    for {
      pred <- sid.preds.toStream
      head = pred.head
      RuleBody(_, body) <- pred.rules
      if !break.flag
      _ = {
        logger.debug("Looking at defined sources for " + head + " <= " + body)
      }
      (src,trg) <- if (!skipSinksAsSources || shouldTryAllSources(body)) {
        considerAllSourceCombinations(head, body, reached, previousTransitions, break)
      } else {
        logger.debug(s"${body.predCalls.size} calls => Will perform incremental instantiation")
        incrementalInstantiation(head, body, reached, break, body.identsOfCalledPreds)
      }
    } yield ((head, trg), (src,body,head))
  }

  @tailrec
  private def computeRefinementFixedPoint(reached : ReachedStates,
                                          transitions: Transitions,
                                          iteration : Int) : RefinementState = {

    if (reached.containsFinalState && mode == OnTheFly) {
      // There is a derivation that reaches a final state, refined language nonempty
      // We only continue the fixed-point computation if we're interested in the full refinement; otherwise we return false
      logger.debug("Reached " + reached.finalStates.head + " => language is non-empty")
      RefinementState(empty = false, reached, Set.empty)
    } else {
      logger.debug("Beginning iteration #" + iteration)
      val iterationResult = performSingleIteration(reached, transitions)
      val newPairs = iterationResult map (_._1)
      val newReachedStates = reached ++ newPairs

      logger.debug("Refinement iteration: #" + iteration + " " + (if (newPairs.isEmpty) "--" else newPairs.mkString(", ")))
      if (reportProgress) println(dateFormat.format(new java.util.Date) + " -- Refinement iteration: #" + iteration + " Discovered " + newPairs.size + " targets; total w/o duplicates: " + newReachedStates.size)

      if (newReachedStates.size == reached.size) {
        // Fixed point reached
        logger.debug("Fixed point: " + newReachedStates.pairs.mkString(", "))
        if (!reached.containsFinalState) {
          logger.debug("=> Language is empty")
        }

        val transitionInstances: Set[TransitionInstance] = if (mode == FullRefinement) {
          // Only extend in FullRefinement mode -- otherwise this is unnecessary extra computation
          transitions.extend(iterationResult).toTransitionInstances
        } else {
          Set.empty
        }

        RefinementState(empty = true, reached, transitionInstances)
      } else {
        // Fixed point not yet reached, recurse
        val updatedTransitions = transitions.extend(iterationResult)
        computeRefinementFixedPoint(newReachedStates, updatedTransitions, iteration + 1)
      }
    }
  }

  private lazy val dateFormat : SimpleDateFormat = new SimpleDateFormat("hh:mm:ss.SSS")

}

object RefinementInstance {

  type RefinementMode = Boolean
  val OnTheFly = false
  val FullRefinement = true
  var DefaultIncrementalFromNumCalls: Int = 6

}
