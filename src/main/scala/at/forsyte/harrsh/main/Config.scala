package at.forsyte.harrsh.main

import at.forsyte.harrsh.refinement.AutomatonTask

import scala.concurrent.duration.{Duration, SECONDS}

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
    oModelFile : Option[String],
    oNumFV : Option[Int],
    reportProgress : Boolean,
    verbose : Boolean,
    debug : Boolean
) {

  def file = oFile.get
  def timeout = oTimeout.get
  def prop = oProp.get
  def unfoldingsReduced = oUnfoldingsReduced.getOrElse(false)
  def unfoldingDepth = oUnfoldingDepth.getOrElse(Config.DefaultDepth)
  def modelFile = oModelFile.get
  def numFV = oNumFV.get

  override def toString : String =
    ("Config[\n    mode = " + mode + ",\n"
      + showOptional(oModelFile, "model file")
      + showOptional(oFile, "sid file")
      + showOptional(oProp, "prop")
      + showOptional(oTimeout, "timeout")
      + showOptional(oUnfoldingsReduced, "unf. reduced")
      + showOptional(oUnfoldingDepth, "unf. depth")
      + showOptional(oNumFV, "num. fv.")
      + (if (verbose) "    verbose\n" else "")
      + (if (reportProgress) "    report progress\n" else "")
      + (if (debug) "    debug\n" else "")
      + "]")

  private def showOptional[A](a : Option[A], label : String) : String = a.map("    " + label + " = " + _ + "\n").getOrElse("")

}

object Config {

  /**
    * Default configuration prior to parsing command-line args
    */
  val DefaultConfig = Config(Help, None, None, None, None, None, None, None, reportProgress = false, verbose = false, debug = false)

  /**
    * Default unfolding depth in unfolding mode
    */
  val DefaultDepth = 3

  /**
    * Timeout used in the preprocessing of SIDs in MC/Entailment modes
    */
  val PreprocessingTimeout = Duration(1200, SECONDS)

  /**
    * Should additional sanity checks be performed (incurring small performance penalties)?
    */
  val HeapAutomataSafeModeEnabled : Boolean = false
}
