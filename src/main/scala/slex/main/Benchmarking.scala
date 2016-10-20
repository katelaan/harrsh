package slex.main

import java.io.File

import slex.heapautomata._
import slex.seplog.inductive.{PureAtom, SID}
import slex.seplog.parsers.{CyclistSIDParser, DefaultSIDParser}
import slex.util.IOUtils._

/**
  * Created by jkatelaa on 10/20/16.
  */
object Benchmarking {

  val PathToDatastructureExamples = "examples" + File.separator + "datastructures"
  val PathToCyclistExamples = "examples" + File.separator + "cyclist"

  val CyclistSuffix = "defs"
  val SidSuffix = "sid"

  def main(args : Array[String]) = {
    runBenchmarks(generateTasks())
  }

  def generateTasks() =
    for {
      file <- getListOfFiles(PathToDatastructureExamples)
      automaton <- Seq(RunHasPointer(), RunTracking(Set(fv(1)), Set()), RunSat(), RunUnsat(), RunEstablishment(), RunNonEstablishment(), RunReachability(fv(1), fv(0)), RunGarbageFreedom(), RunAcyclicity())
    } yield TaskConfig(file.getAbsolutePath, automaton, None)


  def runBenchmarks(tasks : Seq[TaskConfig]): Unit = {

    val globalStartTime = System.currentTimeMillis()
    var verificationTime : Long = 0

    for (task <- tasks) {
      val (sid, ha) = prepareBenchmark(task)
      println("Will run automaton " + ha + " on " + sid)
      val startTime = System.currentTimeMillis()
      val result = RefinementAlgorithms.onTheFlyEmptinessCheck(sid, ha)
      val endTime = System.currentTimeMillis()
      println("Finished in " + (endTime - startTime) + "ms")
      verificationTime += (endTime - startTime)
    }

    val globalEndTime = System.currentTimeMillis()
    println("Completed number of benchmarks: " + tasks.size)
    println("Total time: " + (globalEndTime-globalStartTime) + "ms")
    println("Of which analysis time: " + verificationTime + "ms")

  }

  def prepareBenchmark(task : TaskConfig) : (SID, HeapAutomaton) = {

    val parser = if (task.fileName.endsWith(CyclistSuffix)) {
      println("File ends in .defs, will assume cyclist format")
      CyclistSIDParser.run _
    } else {
      println("Assuming standard SID format")
      DefaultSIDParser.run _
    }

    val content = readFile(task.fileName)

    parser(content) match {
      case Some((sid,numFV)) =>
        (sid, task.decisionProblem.getAutomaton(numFV))
      case None =>
        println("Parsing failed, exiting")
        throw new Exception("Parsing failed during benchmark run for task " + task)
    }

  }

}
