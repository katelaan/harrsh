package at.forsyte.harrsh.parsers.slcomp

import BenchmarkStatus._

sealed trait BenchmarkStatus {

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

object BenchmarkStatus {
  case object Sat extends BenchmarkStatus
  case object Unsat extends BenchmarkStatus
  case object Unknown extends BenchmarkStatus

  def fromString(s : String): BenchmarkStatus = s match {
    case "sat" => Sat
    case "unsat" => Unsat
    case "unknown" => Unknown
    case other => throw new Exception(s"Can't convert $other to problem status")
  }
}
