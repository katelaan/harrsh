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
      // Note: Important to add prefix rather than suffix to correctly deal with quantifiers of the form xi (i.e., that look like free variables)
      val candidate = "_" + varid
      freshName(candidate)
    }

  final def addBoundVarWithOptionalAlphaConversion(varid: String) : Renaming = {
    // Note: We always add an entry for the varid, even if no renaming is necessary
    // This ensures that there are no repeated qvars in the combination of multiple sub-heaps with the same quantified vars
    extendWith(varid, freshName(varid))
  }

}

object Renaming {

  /**
    * Creates a Renaming function whose domain consists of dummy values and whose codomain is equal to the given set of varClashes.
    * When passed as argument to symbolic heap renaming, this will in effect rename all bound variables that appear in varClashes to fresh names.
    * @param varClashes Set of potentially clashing variables
    * @return Renaming with codomain varClashes
    */
  def clashAvoidanceRenaming(varClashes : Seq[String]) = {
    val entries = varClashes.zipWithIndex map {
      case (v,i) => ("@#$%$#@" + i, v)
    }
    MapBasedRenaming(Map() ++ entries)
  }

}
