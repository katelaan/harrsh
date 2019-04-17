package at.forsyte.harrsh.refinement

import java.text.SimpleDateFormat

import RefinementInstance._
import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.heapautomata.HeapAutomaton.Transition
import at.forsyte.harrsh.main.HarrshLogging
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

  case class ReachedStates(pairs: Set[(String, ha.State)], cachedFinalStates: Option[Set[ha.State]] = None) {

    lazy val allReachedStates: Set[ha.State] = pairs.map(_._2)

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

  case class Transitions(underlying: Set[Transition[ha.State]]) {

    type TransitionTargetCombination = (Seq[ha.State], SymbolicHeap, String)
    lazy val combinations: Set[TransitionTargetCombination] = underlying map (t => (t.srcStates, t.body, t.headPredicate))

    def extend(iterationResult: Iterable[Transition[ha.State]]): Transitions = {
      Transitions(underlying ++ iterationResult)
    }

  }

  object Transitions {
    def empty: Transitions = Transitions(Set.empty)
  }

  case class RefinementState(empty: Boolean,
                             reachedStates: ReachedStates,
                             reachedTransitions: Transitions)
  {

    /**
      * Convert refinement state to SID
      * @return The refined SID as well as a flag indicating whether it is empty
      */
    def toSID : (Sid,Boolean) = {
      // Assign suffixes to each state
      val states : Set[ha.State] = reachedStates.allReachedStates
      val stateToIndex : Map[ha.State, Int] = states.toSeq.zipWithIndex.toMap

      val innerRules = for {
        Transition(states, body, _, head, headState) <- reachedTransitions.underlying.toSeq
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
                                       target: Option[ha.State] = None) : Stream[Transition[ha.State]] = {
    val isGoal = mode == OnTheFly && head == pred
    if (callsToInstantiate.isEmpty) {
      val trg = target.get
      if (isGoal && ha.isFinal(trg)) {
        break.flag = true
      }
      Stream(Transition(picked, body, None, head, target.get))
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
                                            break: FlagWrapper): Stream[Transition[ha.State]] = {
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
      transition <- {
        logger.debug("Yes, targets not computed previously, get targets for " + body)
        val trg = ha.getTransitionsFor(src, body, head)
        if (isGoal && trg.exists(t => ha.isFinal(t.headState))) {
          break.flag = true
        }
        trg
      }
    } yield transition
  }

  private def shouldTryAllSources(sh: SymbolicHeap) = {
    // We'll try all combinations if the number of pred calls is small
    !ha.implementsPartialTargets || sh.predCalls.size < incrementalBound
  }

  case class FlagWrapper(var flag: Boolean)

  private def performSingleIteration(reached : ReachedStates,
                                     previousTransitions : Transitions): Stream[Transition[ha.State]] = {
    var break = FlagWrapper(false)
    for {
      pred <- sid.preds.toStream
      head = pred.head
      _ = logger.debug("Computing reachable states for predicate " + head)
      RuleBody(_, body) <- pred.rules
      if !break.flag
      _ = logger.debug(s"[$head]: Looking at defined sources for $head <= $body")
      transition <- if (!skipSinksAsSources || shouldTryAllSources(body)) {
        considerAllSourceCombinations(head, body, reached, previousTransitions, break)
      } else {
        logger.debug(s"[$head]: ${body.predCalls.size} calls => Will perform incremental instantiation")
        incrementalInstantiation(head, body, reached, break, body.identsOfCalledPreds)
      }
    } yield transition
  }

  @tailrec
  private def computeRefinementFixedPoint(reached : ReachedStates,
                                          transitions: Transitions,
                                          iteration : Int) : RefinementState = {

    if (reached.containsFinalState && mode == OnTheFly) {
      // There is a derivation that reaches a final state, refined language nonempty
      // We only continue the fixed-point computation if we're interested in the full refinement; otherwise we return false
      logger.debug("Reached " + reached.finalStates.head + " => language is non-empty")
      RefinementState(empty = false, reached, Transitions.empty)
    } else {
      logger.debug("Beginning iteration #" + iteration)
      val iterationResult = performSingleIteration(reached, transitions)
      val newPairs = iterationResult map (t => (t.headPredicate, t.headState))
      val newReachedStates = reached ++ newPairs

      logger.debug("Refinement iteration: #" + iteration + " " + (if (newPairs.isEmpty) "--" else newPairs.mkString(", ")))
      if (reportProgress) println(dateFormat.format(new java.util.Date) + " -- Refinement iteration: #" + iteration + " Discovered " + newPairs.size + " targets; total w/o duplicates: " + newReachedStates.size)

      if (newReachedStates.size == reached.size) {
        // Fixed point reached
        logger.debug("Fixed point: " + newReachedStates.pairs.mkString(", "))
        if (!reached.containsFinalState) {
          logger.debug("=> Language is empty")
        }

        val newTransitions = if (mode == FullRefinement) {
          // Only extend in FullRefinement mode -- otherwise this is unnecessary extra computation
          transitions.extend(iterationResult)
        } else {
          transitions
        }

        RefinementState(empty = true, reached, newTransitions)
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
