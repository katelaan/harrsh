package at.forsyte.harrsh.main

import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by jens on 2/24/17.
  */
sealed trait ExecutionMode {
  def requiresProp : Boolean = this match {
    case Help => false
    case Decide => true
    case Refine => true
    case Batch => false
    case Show => false
    case Unfold => false
    case Analyze => false
    case ModelChecking => false
    case ParseOnly => false
  }


  def defaultTimeout : Duration = this match {
    case Decide => Duration(120, SECONDS)
    case Batch => Duration(120, SECONDS)
    case Analyze => Duration(5, SECONDS)
    case ModelChecking => Duration(120, SECONDS)
    case ParseOnly => Duration(10, SECONDS)
    case _ => Duration(0, SECONDS)
  }
}

case object Help extends ExecutionMode
case object Decide extends ExecutionMode
case object Refine extends ExecutionMode
case object Batch extends ExecutionMode
case object Show extends ExecutionMode
case object Unfold extends ExecutionMode
case object Analyze extends ExecutionMode
case object ModelChecking extends ExecutionMode
case object ParseOnly extends ExecutionMode