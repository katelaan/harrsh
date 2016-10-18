package slex.smtinteraction

import java.io.{BufferedWriter, File, FileWriter}

import slex.main.{Defaults, SlexLogging}
import slex.models.Stack
import slex.smtsyntax.{CheckSat, GetModel, SmtCommand}

import sys.process._

/**
  * A Z3 wrapper that writes all currently active commands to a file, calls Z3 on that file and parses the result.
  * Incredibly inefficient, as it does not support incremental queries, but possibly useful for debugging,
  * as the file(s) survive the execution of the program and can thus be tried with "standalone" Z3 later.
  *
  * Created by jkatelaa on 9/30/16.
  */
class NaiveZ3Wrapper(pathToZ3 : String = Defaults.PathToZ3, fileName : String = "tmp.smt2") extends SmtWrapper with SlexLogging {

  private lazy val ErrorResult : (SatStatus, Option[Stack]) = (ErrorStatus(), None)

  private var commandStack : Seq[SmtCommand] = Seq()

  override def restart(): Unit = {
    commandStack = Seq()
  }

  override def close(): Unit = {
    // Nothing to close
  }

  override def addCommands(query: Seq[SmtCommand]): Unit = {
    commandStack ++= query
  }

  override def checkSat(): SatStatus= runSmtQuery(commandStack :+ CheckSat())._1

  override def computeModel(): Option[Stack] = runSmtQuery(commandStack :+ CheckSat() :+ GetModel())._2

  private def runSmtQuery(query : Seq[SmtCommand]) : (SatStatus, Option[Stack]) = {
    writeSmtFile(query)
    val command = pathToZ3 + " " + fileName
    val process = Process(command)
    //println("Will run: " + process.toString)

    try {
      val res = process.!!
      Z3ResultParser.run(res).getOrElse {
        logger.info("Z3 returned unparsable result: " + res)
        logger.info("PARSE ERROR")
        ErrorResult
      }
    } catch {
      case e : RuntimeException =>
        logger.info("ERROR IN INTERACTION WITH Z3, " + e)
        ErrorResult
    }
  }

  private def writeSmtFile(input : Seq[SmtCommand]): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(input.mkString("\n"))
    bw.close()
  }
}
