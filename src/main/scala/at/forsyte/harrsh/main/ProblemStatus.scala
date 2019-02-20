package at.forsyte.harrsh.main

import at.forsyte.harrsh.main.ProblemStatus._

sealed trait ProblemStatus {

  def flip: ProblemStatus = this match {
    case Correct => Incorrect
    case Incorrect => Correct
    case Unknown => Unknown
  }

  override def toString: String = this match {
    case Correct => "correct"
    case Incorrect => "incorrect"
    case Unknown => "unknown"
  }

  def toBoolean: Option[Boolean] = this match {
    case Correct => Some(true)
    case Incorrect => Some(false)
    case Unknown => None
  }
}

object ProblemStatus {
  case object Correct extends ProblemStatus
  case object Incorrect extends ProblemStatus
  case object Unknown extends ProblemStatus

  def fromString(s : String): ProblemStatus = s match {
    case "sat" => Correct
    case "unsat" => Incorrect
    case "unknown" => Unknown
    case other => throw new Exception(s"Can't convert $other to problem status")
  }
}
