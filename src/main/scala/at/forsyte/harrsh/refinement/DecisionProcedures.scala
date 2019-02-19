package at.forsyte.harrsh.refinement

import java.io.File

import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.main.{HarrshLogging, MainIO, RefinementQuery}
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
import at.forsyte.harrsh.util.IOUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, TimeoutException}

/**
  * Created by jkatelaa on 10/20/16.
  */
object DecisionProcedures extends HarrshLogging {

  case class AnalysisResult(isEmpty: Boolean, analysisTime: Long, timedOut: Boolean)
  case class AnalysisStatistics(globalStartTime: Long, globalEndTime: Long, analysisTime: Long, timeout: Duration, numTimeouts: Int)

  val PathToDatastructureExamples: String = "examples" + File.separator + "datastructures"
  val PathToCyclistExamples: String = "examples" + File.separator + "cyclist"

  // Uncomment & run this class to generate benchmark suites
  //def main(args : Array[String]): Unit = generateAndPrintInstances()

  def decideInstance(query : RefinementQuery, timeout : Duration, verbose : Boolean, reportProgress : Boolean): AnalysisResult = {
    val (sid, ha) = (query.sid, query.automaton)

    if (verbose) {
      IOUtils.printLinesOf('%', 1)
      println("File: " + query.fileName)
      IOUtils.printLinesOf('%', 1)
      println("Will run automaton " + ha + " on " + sid)
    } else {
      println("Running " + query.taskString + " on " + query.fileName + "...")
    }

    decideInstance(sid, ha, timeout, None, verbose = verbose, reportProgress = reportProgress)
  }

  def decideInstance(sid : SID, ha : HeapAutomaton, timeout : Duration, topLevelQuery: Option[SymbolicHeap] = None, skipSinksAsSources : Boolean = false, verbose : Boolean = false, reportProgress : Boolean = false): AnalysisResult = {
    val startTime = System.currentTimeMillis()

    val f: Future[Boolean] = Future {
      RefinementAlgorithms.onTheFlyRefinementWithEmptinessCheck(sid, ha, topLevelQuery, skipSinksAsSources = skipSinksAsSources, reportProgress = reportProgress)
    }

    val result = try {
      val isEmpty = Await.result(f, timeout)
      val endTime = System.currentTimeMillis()
      if (verbose) println("Finished in " + (endTime - startTime) + "ms")
      AnalysisResult(isEmpty, endTime - startTime, timedOut = false)
    } catch {
      case e : TimeoutException =>
        if (verbose) println("reached timeout (" + timeout + ")")
        AnalysisResult(isEmpty = true, timeout.toMillis, timedOut = true)
    }

    result
  }

  def decideInstances(queries : Seq[RefinementQuery], timeout : Duration, verbose : Boolean, reportProgress : Boolean): (Seq[(RefinementQuery,AnalysisResult)],AnalysisStatistics) = {

    val globalStartTime = System.currentTimeMillis()
    var analysisTime : Long = 0

    var results : List[(RefinementQuery,AnalysisResult)] = Nil
    var numTimeouts : Int = 0

    for (query <- queries) {
      val (sid, ha) = (query.sid, query.automaton)
      if (verbose) {
        IOUtils.printLinesOf('%', 1)
        println("File: " + query.fileNameString)
        IOUtils.printLinesOf('%', 1)
        println("Will run automaton " + ha + " on " + sid)
      } else {
        print("Running " + query.taskString + " on " + query.fileNameString + "... ")
      }

      val startTime = System.currentTimeMillis()

      val f: Future[Boolean] = Future {
        RefinementAlgorithms.onTheFlyRefinementWithEmptinessCheck(sid, ha, reportProgress = reportProgress)
      }

      val result = try {
        val isEmpty = Await.result(f, timeout)
        val endTime = System.currentTimeMillis()
        println("Finished in " + (endTime - startTime) + "ms")
        analysisTime += (endTime - startTime)
        AnalysisResult(isEmpty, endTime - startTime, timedOut = false)
      } catch {
        case e : TimeoutException =>
          println("reached timeout (" + timeout + ")")
          numTimeouts += 1
          AnalysisResult(isEmpty = true, timeout.toMillis, timedOut = true)
      }

      results = (query, result) :: results

    }

    val globalEndTime = System.currentTimeMillis()

    (results.reverse, AnalysisStatistics(globalStartTime, globalEndTime, analysisTime, timeout, numTimeouts))
  }

  def deviationsFromExpectations(results: Seq[(RefinementQuery, AnalysisResult)]): Seq[(RefinementQuery, AnalysisResult)] = {
    results.filter {
      case (config, result) =>
        // The expected result describes the existence of an unfolding with the property, so it should be the negation of isEmpty
        val maybeStatus = config.status.toBoolean
        maybeStatus.nonEmpty && maybeStatus.get == result.isEmpty
    }
  }

  def generateAndPrintInstances() : Unit = {
    // Auto-generate benchmark suite
    println(generateInstances() map (_.toString) mkString "\n")
  }

  private def generateInstances() =
    for {
      automaton <- Seq(RunHasPointer(),
        RunAllocationTracking(Set(Var.defaultFV(1))),
        RunPureTracking(Set(Var.defaultFV(1) =:= NullConst)),
        RunPureTracking(Set(Var.defaultFV(1) =/= NullConst)),
        RunRelaxedTracking(Set(Var.defaultFV(1)), Set(Var.defaultFV(1) =/= NullConst)),
        RunExactTracking(Set(Var.defaultFV(1)), Set()),
        RunSat,
        RunUnsat,
        RunEstablishment,
        RunNonEstablishment,
        RunReachability(Var.defaultFV(1), NullConst),
        RunGarbageFreedom,
        RunMayHaveGarbage,
        RunWeakAcyclicity,
        RunStrongCyclicity,
        RunModulo(0,2),
        RunModulo(5,11),
        RunModulo(126,128),
        RunModulo(127,128))
      file <- IOUtils.getListOfFiles(PathToDatastructureExamples).sortBy(_.getName) //++ getListOfFiles(PathToCyclistExamples).sortBy(_.getName)
    } yield RefinementQuery(file.getAbsolutePath, automaton)

}
