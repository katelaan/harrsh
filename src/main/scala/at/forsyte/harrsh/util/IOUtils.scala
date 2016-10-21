package at.forsyte.harrsh.util

import java.io.{BufferedWriter, File, FileWriter}

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

  def writeFile(fileName : String, lines : Seq[String]): Unit = writeFile(fileName, lines.mkString("\n"))

  def writeFile(fileName : String, content : String): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }

  def printLinesOf(symbol : Char, numLines : Int) = {
    val s : String = symbol.toString
    for (_ <- 1 to numLines) println(s * 80)
  }

}
