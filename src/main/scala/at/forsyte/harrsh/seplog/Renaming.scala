package at.forsyte.harrsh.seplog

/**
  * Created by jkatelaa on 10/17/16.
  */
trait Renaming {

  def apply(s : Var) : Var

  def extendWith(k : Var, v: Var) : Renaming

  def codomain : Set[Var]

  def isDefinedAt(s : Var) : Boolean

  final def freshName(varid: Var): Var =
    if (!codomain.contains(varid)) {
      varid
    } else if (varid < 0) {
      freshName(varid-1)
      // TODO Is it possible to skip directly to the minimum? Want to avoid gaps
      //codomain.min - 1
    } else {
      codomain.max + 1
    }

  final def addBoundVarWithOptionalAlphaConversion(varid: Var) : Renaming = {
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
  def clashAvoidanceRenaming(varClashes : Iterable[Var]) = {
    val entries = varClashes.zipWithIndex map {
      case (v,i) => (Integer.MIN_VALUE + i, v)
    }
    MapBasedRenaming(Map() ++ entries)
  }

}
