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

  def allFilesRecursively(topmostDirectory: String): Stream[File] = {
    allFilesRecursively(new File(topmostDirectory))
  }

  def allFilesRecursively(topmostDirectory: File): Stream[File] = {
    if (topmostDirectory.exists && topmostDirectory.isDirectory) {
      val allLocalFiles = topmostDirectory.listFiles()
      val localFiles = allLocalFiles.filter(_.isFile).toStream
      val subDirs = allLocalFiles.filter(_.isDirectory).toStream
      localFiles ++ subDirs.flatMap(allFilesRecursively)
    } else {
      Stream.empty[File]
    }
  }

  def getListOfFiles(dir: String): List[File] = {
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

  def writeFile(fileName : String, lines : Seq[String]): Unit = writeFile(fileName, lines.mkString("", "\n", "\n"))

  def writeFile(fileName : String, content : String): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }

  def mkDir(fileName : String) : Unit = {
    val path = java.nio.file.Paths.get(fileName)
    java.nio.file.Files.createDirectories(path)
  }

  def printLinesOf(symbol : Char, numLines : Int): Unit = {
    val s : String = symbol.toString
    for (_ <- 1 to numLines) println(s * 80)
  }

  def printIf(condition : => Boolean)(a : Any) : Unit = if (condition) println(a)

}
