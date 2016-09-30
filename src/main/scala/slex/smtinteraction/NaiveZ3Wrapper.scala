package slex.smtinteraction

import java.io.{BufferedWriter, File, FileWriter}

import slex.main.Defaults
import slex.smtsyntax.SmtFormula
import sys.process._

/**
  * Created by jkatelaa on 9/30/16.
  */
class NaiveZ3Wrapper(pathToZ3 : Option[String]) extends SmtWrapper {

  private val FileName = "tmp.smt2"

  private val path = pathToZ3 getOrElse Defaults.PathToZ3

  override def runSmtQuery(query : Seq[SmtFormula]) : String = {
    writeSmtFile(query)
    val command = path + " " + FileName
    val process = Process(command)
    println("Will run: " + process.toString)
    process.!!
  }

  private def writeSmtFile(input : Seq[SmtFormula]): Unit = {
    val file = new File(FileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(input.mkString("\n"))
    bw.close()
  }

}
