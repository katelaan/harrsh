package at.forsyte.harrsh.seplog.parsers

import at.forsyte.harrsh.heapautomata.DecisionProcedures._
import at.forsyte.harrsh.util.IOUtils._
import at.forsyte.harrsh.main.MainIO._

/**
  * Created by jkatelaa on 10/20/16.
  */
object ParseSIDExampleFolder {


  def main(args : Array[String]) = {

    printLinesOf('%', 2)
    println("Parse examples in SID-Format")
    printLinesOf('%', 2)
    parseAllFiles(PathToDatastructureExamples, SidSuffix, DefaultSIDParser.run)

    printLinesOf('%', 2)
    println("Parse examples in Cyclist-Format")
    printLinesOf('%', 2)
    parseAllFiles(PathToCyclistExamples, CyclistSuffix, CyclistSIDParser.run)

  }


  def parseAllFiles[A](dir : String, ext : String, parseMethod : String => A): Unit = {
    for {
      f <- getListOfFiles(dir)
      if f.getName.endsWith(ext)
    } {
      val content = readFile(f.getAbsolutePath)
      val sid = parseMethod(content)
      printLinesOf('#', 1)
      println("#### " + f.getName + " ####" + "#" * (70 - f.getName.length))
      printLinesOf('#', 1)
      println(sid)
    }
  }
}
