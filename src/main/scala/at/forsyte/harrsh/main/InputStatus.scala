package at.forsyte.harrsh.main

import at.forsyte.harrsh.main.InputStatus._

sealed trait InputStatus {

  override def toString: String = this match {
    case Sat => "sat"
    case Unsat => "unsat"
    case Unknown => "unknown"
  }

  def toBoolean: Option[Boolean] = this match {
    case Sat => Some(true)
    case Unsat => Some(false)
    case Unknown => None
  }
}

object InputStatus {
  case object Sat extends InputStatus
  case object Unsat extends InputStatus
  case object Unknown extends InputStatus

  def fromString(s : String): InputStatus = s match {
    case "sat" => Sat
    case "unsat" => Unsat
    case "unknown" => Unknown
    case other => throw new Exception(s"Can't convert $other to problem status")
  }
}
