package at.forsyte.harrsh.refinement

import java.io.File

import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.main.{HarrshLogging, MainIO, TaskConfig}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.util.IOUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, TimeoutException}

/**
  * Created by jkatelaa on 10/20/16.
  */
object DecisionProcedures extends HarrshLogging {

  //type Result = (Boolean,Long)
  case class AnalysisResult(isEmpty: Boolean, analysisTime: Long, timedOut: Boolean)
  case class AnalysisStatistics(globalStartTime: Long, globalEndTime: Long, analysisTime: Long, timeout: Duration, numTimeouts: Int)

  val PathToDatastructureExamples = "examples" + File.separator + "datastructures"
  val PathToCyclistExamples = "examples" + File.separator + "cyclist"

  // Uncomment & run this class to generate benchmark suites
  //def main(args : Array[String]) = generateAndPrintInstances()


  def decideInstance(task : TaskConfig, timeout : Duration, verbose : Boolean, reportProgress : Boolean): AnalysisResult = {
    val (sid, ha) = MainIO.getSidAndAutomaton(task.fileName, task.decisionProblem)

    if (verbose) {
      IOUtils.printLinesOf('%', 1)
      println("File: " + task.fileName)
      IOUtils.printLinesOf('%', 1)
      println("Will run automaton " + ha + " on " + sid)
    } else {
      print("Running " + task.decisionProblem + " on " + task.fileName + "...")
    }

    decideInstance(sid, ha, timeout, verbose, reportProgress)
  }

  def decideInstance(sid : SID, ha : HeapAutomaton, timeout : Duration, verbose : Boolean, reportProgress : Boolean): AnalysisResult = {
    val startTime = System.currentTimeMillis()

    val f: Future[Boolean] = Future {
      RefinementAlgorithms.onTheFlyRefinementWithEmptinessCheck(sid, ha, reportProgress = reportProgress)
    }

    val result = try {
      val isEmpty = Await.result(f, timeout)
      val endTime = System.currentTimeMillis()
      println("Finished in " + (endTime - startTime) + "ms")
      AnalysisResult(isEmpty, endTime - startTime, timedOut = false)
    } catch {
      case e : TimeoutException =>
        println("reached timeout (" + timeout + ")")
        AnalysisResult(isEmpty = true, timeout.toMillis, timedOut = true)
    }

    result
  }

  def decideInstances(tasks : Seq[TaskConfig], timeout : Duration, verbose : Boolean, reportProgress : Boolean): (Seq[(TaskConfig,AnalysisResult)],AnalysisStatistics) = {

    val globalStartTime = System.currentTimeMillis()
    var analysisTime : Long = 0

    var results : List[(TaskConfig,AnalysisResult)] = Nil
    var numTimeouts : Int = 0

    for (task <- tasks) {
      val (sid, ha) = MainIO.getSidAndAutomaton(task.fileName, task.decisionProblem)
      if (verbose) {
        IOUtils.printLinesOf('%', 1)
        println("File: " + task.fileName)
        IOUtils.printLinesOf('%', 1)
        println("Will run automaton " + ha + " on " + sid)
      } else {
        print("Running " + task.decisionProblem + " on " + task.fileName + "...")
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

      results = (task, result) :: results

    }

    val globalEndTime = System.currentTimeMillis()

    (results.reverse, AnalysisStatistics(globalStartTime, globalEndTime, analysisTime, timeout, numTimeouts))
  }

  def prepareInstanceForAnalysis(task : TaskConfig) : (SID, HeapAutomaton) = {
    val sid = MainIO.getSidFromFile(task.fileName)
    (sid, task.decisionProblem.getAutomaton(sid.numFV))
  }

  def deviationsFromExpectations(results: Seq[(TaskConfig, AnalysisResult)]): Seq[(TaskConfig, AnalysisResult)] = {
    results.filter{
      case (config, result) =>
        // The expected result describes the existence of an unfolding with the property, so it should be the negation of isEmpty
        config.expectedResult.isDefined && config.expectedResult.get == result.isEmpty
    }
  }

  def generateAndPrintInstances() : Unit = {
    // Auto-generate benchmark suite
    println(generateInstances() map (_.toString) mkString "\n")
  }

  private def generateInstances() =
    for {
      automaton <- Seq(RunHasPointer(), RunTracking(Set(Var.mkVar(1)), Set()), RunSat(), RunUnsat(), RunEstablishment(), RunNonEstablishment(), RunReachability(Var.mkVar(1), Var.mkVar(0)), RunGarbageFreedom(), RunMayHaveGarbage(), RunWeakAcyclicity(), RunStrongCyclicity(), RunModulo(0,2), RunModulo(5,11), RunModulo(126,128), RunModulo(127,128))
      file <- IOUtils.getListOfFiles(PathToDatastructureExamples).sortBy(_.getName) //++ getListOfFiles(PathToCyclistExamples).sortBy(_.getName)
    } yield TaskConfig(file.getAbsolutePath, automaton, None)

}
