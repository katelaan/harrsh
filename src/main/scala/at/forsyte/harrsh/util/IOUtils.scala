package at.forsyte.harrsh.util

import java.io.{BufferedWriter, File, FileWriter}

import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by jkatelaa on 10/20/16.
  */
object IOUtils {

  def getCurrentDirectory = new java.io.File(".").getCanonicalPath

  def printWarningToConsole(warning : String): Unit = {
    println(Console.BLUE + "Warning: " + warning + Console.RESET)
  }

  def getListOfFiles(dir: String) : List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  /**
    * Searches the given list of directories for a file of the given name, returning first hit (as path + filename)
    * @param filename File to find
    * @param listOfDirs Directories to search
    * @return Some full path or none
    */
  @tailrec def findFileIn(filename : String, listOfDirs : Seq[String]) : Option[String] = {
    if (listOfDirs.isEmpty) None else {
      val path = listOfDirs.head + "/" + filename
      if (new File(path).exists()) Some(path) else findFileIn(filename, listOfDirs.tail)
    }
  }

  def listOfFilesIn(listOfDirs : Seq[String]) : List[File] = {
    listOfDirs.foldLeft(List.empty[File]){
      (acc, dir) => acc ++ getListOfFiles(dir)
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

  def computeColumnLength(entries : Seq[Any], minLength : Int) : Int = Math.max(minLength,entries.map(_.toString.size).max + 1)

  def toTable(headings: Seq[String], cols: Seq[Int], entries: Seq[Seq[String]]) : String = {
    val delimLine = IOUtils.delimLine(cols)

    val lines = for {
      entry <- entries
    } yield IOUtils.inColumns(entry zip cols)

    delimLine + "\n" + IOUtils.inColumns(headings zip cols) + "\n" + delimLine + "\n" + lines.mkString("\n")+"\n" + delimLine
  }

  private def inColumns(cols : Seq[(String,Int)]) : String = if (cols.isEmpty) "|" else "|" + " "*Math.max(0,cols.head._2 - cols.head._1.length) + cols.head._1 + inColumns(cols.tail)
  private def delimLine(cols : Seq[Int]) = "+" + "-"*(cols.sum+cols.size-1) + "+"

  def printIf(condition : => Boolean)(a : Any) : Unit = if (condition) println(a)

}
