package slex.util

import java.io.File

import scala.io.Source

/**
  * Created by jkatelaa on 10/20/16.
  */
object IOUtils {

  def getListOfFiles(dir: String) : List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def readFile(filename : String) : String = {
    val source = Source.fromFile(filename)

    try {
      source.mkString
    } catch {
      case e : Exception =>
        println("Could not read file '" + filename + "'")
        ""
    } finally {
      source.close()
    }
  }

  def printLinesOf(symbol : Char, numLines : Int) = {
    val s : String = symbol.toString
    for (_ <- 1 to numLines) println(s * 80)
  }

}
