package at.forsyte.harrsh.main

import at.forsyte.harrsh.heapautomata.AutomatonTask

import scala.concurrent.duration.Duration

/**
  * Created by jens on 2/24/17.
  */
case class Config(
    mode : ExecutionMode,
    oFile : Option[String],
    oProp : Option[AutomatonTask],
    oTimeout : Option[Duration],
    oUnfoldingsReduced : Option[Boolean],
    oUnfoldingDepth : Option[Int],
    reportProgress : Boolean,
    verbose : Boolean,
    debug : Boolean
) {

  def file = oFile.get
  def timeout = oTimeout.get
  def prop = oProp.get
  def unfoldingsReduced = oUnfoldingsReduced.getOrElse(false)
  def unfoldingDepth = oUnfoldingDepth.getOrElse(Config.DefaultDepth)

  override def toString : String =
    ("Config[\n    mode = " + mode + ",\n"
      + showOptional(oFile, "file")
      + showOptional(oProp, "prop")
      + showOptional(oTimeout, "timeout")
      + showOptional(oUnfoldingsReduced, "unf. reduced")
      + showOptional(oUnfoldingDepth, "unf. depth")
      + (if (verbose) "    verbose\n" else "")
      + (if (reportProgress) "    report progress\n" else "")
      + (if (debug) "    debug\n" else "")
      + "]")

  private def showOptional[A](a : Option[A], label : String) : String = a.map("    " + label + " = " + _ + "\n").getOrElse("")

}

object Config {
  val DefaultConfig = Config(Help(), None, None, None, None, None, false, false, false)

  val DefaultDepth = 3

  val HeapAutomataSafeModeEnabled : Boolean = false
}
