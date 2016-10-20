package slex.seplog.parsers

import java.io.File

import slex.util.IOUtils._

/**
  * Created by jkatelaa on 10/20/16.
  */
object ParseExamples {

  val PathToDatastructureExamples = "examples" + File.separator + "datastructures"
  val PathToCyclistExamples = "examples" + File.separator + "cyclist"

  def main(args : Array[String]) = {

    printLinesOf('%', 2)
    println("Parse examples in SID-Format")
    printLinesOf('%', 2)
    parseAllFiles(PathToDatastructureExamples, "sid", DefaultSIDParser.run)

    printLinesOf('%', 2)
    println("Parse examples in Cyclist-Format")
    printLinesOf('%', 2)
    parseAllFiles(PathToCyclistExamples, "defs", CyclistSIDParser.run)

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
