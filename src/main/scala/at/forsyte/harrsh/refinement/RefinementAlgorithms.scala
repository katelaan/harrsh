package at.forsyte.harrsh.refinement

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.StringUtils

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

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
  def onTheFlyRefinementWithEmptinessCheck(sid : SID, ha : HeapAutomaton, query: Option[SymbolicHeap] = None, skipSinksAsSources: Boolean = false, reportProgress : Boolean = false) : Boolean = {
    RefinementInstance(sid, ha, query, mode = RefinementInstance.OnTheFly, skipSinksAsSources = skipSinksAsSources, reportProgress = reportProgress).run.empty
  }

  /**
    * Refines SID
    * @param sid The SID to refine
    * @param ha Automaton by which we refine
    * @param timeout Return None after this timeout has passed
    * @param reportProgress Periodically report the number of iterations
    * @return The refined SID + emptiness flag (true iff empty) or None in case of timeout
    */
  def refineSID(sid: SID, ha: HeapAutomaton, timeout: Duration, reportProgress: Boolean): Option[(SID,Boolean)] = {
    val f: Future[(SID,Boolean)] = Future {
      RefinementInstance(sid, ha, mode = RefinementInstance.FullRefinement, reportProgress = reportProgress).run.toSID
    }

    try {
      val sid = Await.result(f, timeout)
      Some(sid)
    } catch {
      case e: TimeoutException =>
        println("reached timeout (" + timeout + ")")
        None
    }
  }

  /**
    * (Task to perform, is refined SID empty (or None if timeout), witness if nonempty)
   */
  case class AnalysisResult(task : AutomatonTask, result : Option[Boolean], witness : Option[SymbolicHeap])

  def performFullAnalysis(sid: SID, timeout: Duration, verbose : Boolean): Unit = {

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

    val shCol : Int = Math.max(40, results.map(_.witness.toString.length).max - 5)
    val cols = Seq(20,20,shCol)
    val headings = Seq("Property", "Result", "Witness")
    val entries : Seq[Seq[String]] = for {
      AnalysisResult(task,res,witness) <- results
    } yield Seq(task.toString, res.map(task.resultToString).getOrElse("TO / ERR"), witness.map(_.toString).getOrElse("-"))
    println(StringUtils.toTable(headings, cols, entries))

  }

  private def analyze(task : AutomatonTask, sid : SID, timeout : Duration, verbose : Boolean) : AnalysisResult = {
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
          val w = SIDUnfolding.firstReducedUnfolding(refinedSid)
          println("Witness: " + w)
          Some(w)
        } else {
          None
        }

        AnalysisResult(task, Some(empty), witness)
    }
  }

}