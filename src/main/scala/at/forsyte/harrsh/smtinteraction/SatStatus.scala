package at.forsyte.harrsh.smtinteraction

/**
  * Created by jkatelaa on 10/4/16.
  */
sealed trait SatStatus {

  def isSat : Boolean = this match {
    case Sat() => true
    case _ => false
  }

  def isError : Boolean = this match {
    case ErrorStatus() => true
    case _ => false
  }

}

case class Sat() extends SatStatus
case class UnSat() extends SatStatus
case class ErrorStatus() extends SatStatus
