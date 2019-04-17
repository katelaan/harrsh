package at.forsyte.harrsh.refinement

import at.forsyte.harrsh.heapautomata.HeapAutomaton.Transition
import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators
import at.forsyte.harrsh.util.StringUtils._

import scala.concurrent.duration.Duration

/**
  * Created by jens on 10/15/16.
  */

object RefinementAlgorithms {

  /**
    * On the fly refinement, stopping as soon as a final state for the top-level query (if given) or start predicate is reachable
    * @param sid SID to refine
    * @param ha Automaton by which we refine
    * @param query Top-level query, if any
    * @param reportProgress Periodically report the number of iterations
    * @return true iff empty
    */
  def onTheFlyRefinementWithEmptinessCheck(sid : SidLike, ha : HeapAutomaton, query: Option[SymbolicHeap] = None, incrementalFromNumCalls: Option[Int] = None, skipSinksAsSources: Boolean = false, reportProgress : Boolean = false) : Boolean = {
    RefinementInstance(sid, ha, query, RefinementInstance.OnTheFly, incrementalFromNumCalls, skipSinksAsSources, reportProgress).run.empty
  }

  /**
    * Refines SID
    * @param sid The SID to refine
    * @param ha Automaton by which we refine
    * @param timeout Return None after this timeout has passed
    * @param reportProgress Periodically report the number of iterations
    * @return The refined SID + emptiness flag (true iff empty) or None in case of timeout
    */
  def refineSID(sid: SidLike, ha: HeapAutomaton, timeout: Duration, incrementalFromNumCalls: Option[Int] = None, reportProgress: Boolean = false): Option[(Sid,Boolean)] = {
    Combinators.tillTimeout(timeout){
      () => RefinementInstance(sid, ha, None, RefinementInstance.FullRefinement, incrementalFromNumCalls, skipSinksAsSources = false, reportProgress).run.toSID
    }.map(_._1)
  }
  
  def fullRefinementTrace(sid: SidLike, ha: HeapAutomaton, reportProgress: Boolean = false): (Map[String, Set[ha.State]], Map[String,Set[Transition[ha.State]]]) = {
    // TODO: Make this more readable & reduce code duplication wrt. allReachableStates
    val res = RefinementInstance(sid, ha, None, RefinementInstance.FullRefinement, None, skipSinksAsSources = false, reportProgress).run
    val typedResultStates = res.reachedStates.pairs.map {
      case (str, state) => (str, state.asInstanceOf[ha.State])
    }
    val statesByPred: Map[String, Set[ha.State]] = typedResultStates.groupBy(_._1).mapValues(pairs => pairs.map(_._2))
    val transitionMap: Map[String, Set[Transition[ha.State]]] = {
      res.reachedTransitions.underlying.groupBy(_.headPredicate) mapValues (_ map (_.asInstanceOf[Transition[ha.State]]))
    }
    (statesByPred, transitionMap)
  }

  def allReachableStates(sid: SidLike, ha: HeapAutomaton, reportProgress: Boolean): Map[String, Set[ha.State]] = {
    val res = RefinementInstance(sid, ha, None, RefinementInstance.FullRefinement, None, skipSinksAsSources = false, reportProgress).run
    val typedResultStates = res.reachedStates.pairs.map {
      case (str, state) => (str, state.asInstanceOf[ha.State])
    }
    typedResultStates.groupBy(_._1).mapValues(pairs => pairs.map(_._2))
  }

  def refineSID(sid: Sid, ha: HeapAutomaton, reportProgress: Boolean): (Sid,Boolean) = {
    RefinementInstance(sid, ha, None, RefinementInstance.FullRefinement, None, skipSinksAsSources = false, reportProgress).run.toSID
  }

  /**
    * (Task to perform, is refined SID empty (or None if timeout), witness if nonempty)
   */
  case class AnalysisResult(task : AutomatonTask, result : Option[Boolean], witness : Option[SymbolicHeap])

  def performFullAnalysis(sid: SidLike, timeout: Duration, verbose : Boolean): Unit = {

    val tasks : Seq[AutomatonTask] = Seq(RunSat, RunUnsat, RunEstablishment, RunNonEstablishment, RunMayHaveGarbage, RunGarbageFreedom, RunWeakAcyclicity, RunStrongCyclicity)

    println("Beginning analysis...")
    val results : Seq[AnalysisResult] = for (task <- tasks) yield {
      try {
        analyze(task, sid, timeout, verbose)
      } catch {
        case e : Exception =>
          println("An error occurred during analysis of " + task + ":\n" + e.toString)
          AnalysisResult(task, None, None)
      }
    }
    println("Finished analysis.")
    println

    // TODO Abstract printing result tables into its own function? (Compare Benchmarking.printBenchmarkResults)
    println("Analysis results for: " + sid)
    println

    val headings = Seq("Property", "Result", "Witness")
    val minColLengths = Seq(20,20,40)
    val alignment = Seq(AlignRight, AlignRight, AlignLeft)
    val entries : Seq[Seq[String]] = for {
      AnalysisResult(task,res,witness) <- results
    } yield Seq(task.toString, res.map(task.resultToString).getOrElse("TO / ERR"), witness.map(_.toString).getOrElse("-"))
    val config = TableConfig(headings, minColLengths, alignment)
    println(toTable(config, entries))

  }

  private def analyze(task : AutomatonTask, sid : SidLike, timeout : Duration, verbose : Boolean) : AnalysisResult = {
    val refined = refineSID(sid, task.getAutomaton, timeout, reportProgress = false)
    refined match {
      case None =>
        println(task + " did not finish within timeout (" + timeout.toSeconds + "s)")
        AnalysisResult(task, None, None)
      case Some((refinedSid,empty)) =>
        println("Finished " + task + ": " + task.resultToString(empty))
        if (verbose) {
          println("Refined SID:\n" + refinedSid)
        }

        val witness : Option[SymbolicHeap] = if (!empty) {
          val w = SidUnfolding.firstReducedUnfolding(refinedSid)
          println("Witness: " + w)
          Some(w)
        } else {
          None
        }

        AnalysisResult(task, Some(empty), witness)
    }
  }

}