package at.forsyte.harrsh.main

import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by jens on 2/24/17.
  */
sealed trait ExecutionMode {
  def requiresProp : Boolean = this match {
    case Help() => false
    case Decide() => true
    case Refine() => true
    case Batch() => false
    case Show() => false
    case Unfold() => false
    case Analyze() => false
    case ModelChecking() => false
  }


  def defaultTimeout : Duration = this match {
    case Decide() => Duration(120, SECONDS)
    case Batch() => Duration(120, SECONDS)
    case Analyze() => Duration(5, SECONDS)
    case ModelChecking() => Duration(120, SECONDS)
    case _ => Duration(0, SECONDS)
  }
}

case class Help() extends ExecutionMode
case class Decide() extends ExecutionMode
case class Refine() extends ExecutionMode
case class Batch() extends ExecutionMode
case class Show() extends ExecutionMode
case class Unfold() extends ExecutionMode
case class Analyze() extends ExecutionMode
case class ModelChecking() extends ExecutionMode