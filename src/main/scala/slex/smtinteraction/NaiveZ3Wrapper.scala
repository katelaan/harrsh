package slex.smtinteraction

import java.io.{BufferedWriter, File, FileWriter}

import slex.main.Defaults
import slex.smtsyntax.SmtCommand
import sys.process._

/**
  * Created by jkatelaa on 9/30/16.
  */
class NaiveZ3Wrapper(pathToZ3 : Option[String]) extends SmtWrapper {

  private val FileName = "tmp.smt2"

  private val path = pathToZ3 getOrElse Defaults.PathToZ3

  override def runSmtQuery(query : Seq[SmtCommand]) : SmtOutput = {
    writeSmtFile(query)
    val command = path + " " + FileName
    val process = Process(command)
    println("Will run: " + process.toString)

    var errors : List[String] = Nil
    var msgs : List[String] = Nil
    val logger = ProcessLogger(x =>  msgs = msgs ++ List(x), x => errors = errors ++ List(x))

    try {
      val res = process.!!(logger)
      Z3ResultParser.run(res).getOrElse{
        println("Z3 returned unparsable result: " + res)
        println("PARSE ERROR")
        (SmtError(), None)
      };
    } catch {
      case e : RuntimeException =>
        println("ERROR IN INTERACTION WITH Z3, " + e)
        println("Output: " + msgs.mkString("\n"))
        println("Errors: " + errors.mkString("\n"))
        (SmtError(), None)
    }
  }

  private def writeSmtFile(input : Seq[SmtCommand]): Unit = {
    val file = new File(FileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(input.mkString("\n"))
    bw.close()
  }

}
