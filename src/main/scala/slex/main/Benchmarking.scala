package slex.main

import java.io.File

import slex.heapautomata._
import slex.seplog.inductive.SID
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

  type Result = (Boolean,Long)

  def main(args : Array[String]) = {

    val tasks = readTasksFromFile("examples/all-benchmarks.bms")
    //runBenchmarks(tasks)

    //println(generateTasks() map (_.toString) mkString ("\n"))

  }

  def readTasksFromFile(filename : String) : Seq[TaskConfig] = {
    val content = readFile(filename)
    val lines = content.split('\n')
    val otasks = lines map TaskConfig.fromString

    if (otasks.exists(_.isEmpty)) {
      println(otasks.mkString("\n"))
      throw new Exception("Error while parsing benchmarks")
    } else {
      otasks map (_.get)
    }
  }

  def generateTasks() =
    for {
      automaton <- Seq(RunHasPointer(), RunTracking(Set(fv(1)), Set()), RunSat(), RunUnsat(), RunEstablishment(), RunNonEstablishment(), RunReachability(fv(1), fv(0)), RunGarbageFreedom(), RunAcyclicity())
      file <- getListOfFiles(PathToDatastructureExamples).sortBy(_.getName) ++ getListOfFiles(PathToCyclistExamples).sortBy(_.getName)
    } yield TaskConfig(file.getAbsolutePath, automaton, None)


  def printBenchmarkResults(results: List[(TaskConfig, Result)]): Unit = {

    val cols = Seq(30,20,20,10)
    val headings = Seq("file", "task", "result", "time")
    val delimLine = "+" + "-"*(cols.sum+cols.size-1) + "+"

    println(delimLine)
    println(inColumns(headings zip cols))
    println(delimLine)
    for ( (task,res) <- results ) {
      val content : Seq[String] = Seq(task.fileName.split("/").last, task.decisionProblem.toString, task.decisionProblem.resultToString(res._1), ""+res._2)
      println(inColumns(content zip cols))
    }
    println(delimLine)

  }

  def inColumns(cols : Seq[(String,Int)]) : String = if (cols.isEmpty) "|" else "|" + (" "*(Math.max(0,cols.head._2 - cols.head._1.length))) + cols.head._1 + inColumns(cols.tail)

  def runBenchmarks(tasks : Seq[TaskConfig]): Unit = {

    val globalStartTime = System.currentTimeMillis()
    var verificationTime : Long = 0

    var results : List[(TaskConfig,Result)] = Nil

    for (task <- tasks) {
      val (sid, ha) = prepareBenchmark(task)
      printLinesOf('%', 1)
      println("File: " + task.fileName)
      printLinesOf('%', 1)
      println("Will run automaton " + ha + " on " + sid)
      val startTime = System.currentTimeMillis()
      val isEmpty = RefinementAlgorithms.onTheFlyEmptinessCheck(sid, ha)
      val endTime = System.currentTimeMillis()
      println("Finished in " + (endTime - startTime) + "ms")

      verificationTime += (endTime - startTime)
      val result = (isEmpty, endTime - startTime)
      results = (task, result) :: results
    }

    val globalEndTime = System.currentTimeMillis()
    println("FINISHED BENCHMARK SUITE")
    println("Completed number of benchmarks: " + tasks.size)
    println("Total time: " + (globalEndTime-globalStartTime) + "ms")
    println("Of which analysis time: " + verificationTime + "ms")

    printBenchmarkResults(results.reverse)

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