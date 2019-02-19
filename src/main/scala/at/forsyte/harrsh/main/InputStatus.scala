package at.forsyte.harrsh.main

import at.forsyte.harrsh.main.InputStatus._

sealed trait InputStatus {

  override def toString: String = this match {
    case Correct => "sat"
    case Incorrect => "unsat"
    case Unknown => "unknown"
  }

  def toBoolean: Option[Boolean] = this match {
    case Correct => Some(true)
    case Incorrect => Some(false)
    case Unknown => None
  }
}

object InputStatus {
  case object Correct extends InputStatus
  case object Incorrect extends InputStatus
  case object Unknown extends InputStatus

  def fromString(s : String): InputStatus = s match {
    case "sat" => Correct
    case "unsat" => Incorrect
    case "unknown" => Unknown
    case other => throw new Exception(s"Can't convert $other to problem status")
  }
}
