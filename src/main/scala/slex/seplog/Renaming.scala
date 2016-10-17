package slex.seplog

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/17/16.
  */
trait Renaming {

  def apply(s : String) : String

  def extendWith(k : String, v: String) : Renaming

  def codomain : Set[String]

  @tailrec
  final def freshName(varid: String): String =
    if (!codomain.contains(varid)) varid else {
      val candidate = varid + "0"
      freshName(candidate)
    }

  final def withOptionalAlphaConversion(varid: String) : Renaming = {
    if (codomain.contains(varid)) extendWith(varid, freshName(varid)) else this
  }

}
