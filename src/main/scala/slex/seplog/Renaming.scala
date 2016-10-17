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

  final def addBoundVarWithOptionalAlphaConversion(varid: String) : Renaming = {
    // Note: We always add an entry for the varid, even if no renaming is necessary
    // This ensures that there are no repeated qvars in the combination of multiple sub-heaps with the same quantified vars
    extendWith(varid, freshName(varid))
  }

}
