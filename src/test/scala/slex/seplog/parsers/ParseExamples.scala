package slex.seplog.parsers

import java.io.File

import slex.util.IOUtils._

/**
  * Created by jkatelaa on 10/20/16.
  */
object ParseExamples {

  val PathToDatastructureExamples = "examples" + File.separator + "datastructures"

  def main(args : Array[String]) = {

    for (f <- getListOfFiles(PathToDatastructureExamples)) {
      val content = readFile(f.getAbsolutePath)
      val sid = DefaultSIDParser.run(content)
      println("#"*80)
      println("#### " + f.getName + " ####" + "#"*(70-f.getName.length))
      println("#"*80)
      println(sid)
    }

  }




}
