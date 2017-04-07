package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.refinement.DecisionProcedures._
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
    parseAllFiles(PathToDatastructureExamples, SidSuffix, SIDParsers.DefaultSIDParser.runOnSID)

    printLinesOf('%', 2)
    println("Parse examples in Cyclist-Format")
    printLinesOf('%', 2)
    parseAllFiles(PathToCyclistExamples, CyclistSuffix, SIDParsers.CyclistSIDParser.runOnSID)

  }


  def parseAllFiles[A](dir : String, ext : String, parseMethod : (String,Boolean) => A): Unit = {
    for {
      f <- getListOfFiles(dir)
      if f.getName.endsWith(ext)
    } {
      val content = readFile(f.getAbsolutePath)
      val sid = parseMethod(content,true)
      printLinesOf('#', 1)
      println("#### " + f.getName + " ####" + "#" * (70 - f.getName.length))
      printLinesOf('#', 1)
      println(sid)
    }
  }
}
