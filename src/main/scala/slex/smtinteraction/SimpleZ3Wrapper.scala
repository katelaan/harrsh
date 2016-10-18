package slex.smtinteraction
import java.io._
import java.util.Scanner

import slex.main.{Defaults, SlexLogging}
import slex.models.Stack
import slex.smtsyntax.{CheckSat, GetModel, RawCommand, Reset, SmtCommand}


/**
  * Simple wrapper for Z3 that runs Z3 in interactive mode, feeds commands to it and parses its results;
  * no access to Z3's Java API.
  */
class SimpleZ3Wrapper(pathToZ3 : String = Defaults.PathToZ3) extends SmtWrapper with SlexLogging {

  // The only way I found to force Z3 to write to the input stream is to add an echo statement. As a side benefit, this makes detecting the end of Z3's output straightforward
  private val DoneCommand = RawCommand("echo \"done\"")

  private val pb = new ProcessBuilder()
  pb.command(pathToZ3, "-in")
  pb.redirectErrorStream(true)

  val process = pb.start()

  private val stdin : OutputStream= process.getOutputStream
  private val stdout : InputStream = process.getInputStream

  //val reader : BufferedReader = new BufferedReader(new InputStreamReader(stdout))
  val writer : BufferedWriter = new BufferedWriter(new OutputStreamWriter(stdin))

  override def restart(): Unit = sendToZ3(Seq(Reset()))

  override def close() = {
    logger.debug("Closing Z3 process")
    writer.close()
    //reader.close()
    stdin.close()
    stdout.close()
    process.destroy()
  }

  override def addCommands(query: Seq[SmtCommand]) = {
    sendToZ3(query)
  }

  override def checkSat(): SatStatus = runSmtQuery(Seq(CheckSat(), DoneCommand))._1

  override def computeModel(): Option[Stack] = runSmtQuery(Seq(CheckSat(), GetModel(), DoneCommand))._2

  private def runSmtQuery(query: Seq[SmtCommand]): (SatStatus,Option[Stack]) = {
    sendToZ3(query)

    val scanner = new Scanner(stdout)
    var res : StringBuilder = new StringBuilder

    var abort = false
    while (!abort && scanner.hasNextLine) {
      val line = scanner.nextLine()
      logger.debug("%%% Z3 %%% " + line)

      if (line.equals("done"))
        abort = true
      else
        res.append(line + "\n")
    }

    Z3ResultParser.run(res.toString).getOrElse{
      logger.info("Z3 returned unparsable result: " + res)
      logger.info("PARSE ERROR")
      (ErrorStatus(), None)
    }

  }

  def sendToZ3(query: Seq[SmtCommand]): Unit = {
    logger.info("Sending commands " + query.mkString("\n"))
    writer.write(query.mkString("\n"))
    writer.flush()
  }

}
