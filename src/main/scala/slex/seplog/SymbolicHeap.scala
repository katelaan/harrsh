package slex.seplog

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap(pure : Seq[PureAtom], spatial: Seq[SpatialAtom], qvars : Seq[String]) {

  override def toString = {
    val prefix = qvars map ("\u2203"+_) mkString(" ")
    val spatialString = spatial.mkString(" * ")
    val pureString = if (pure.isEmpty) "" else pure.mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString
  }

}

object SymbolicHeap {

  def combineHeaps(phi : Option[SymbolicHeap], psi : Option[SymbolicHeap]) : Option[SymbolicHeap] = {
    for {
      SymbolicHeap(pure, spatial, qvars) <- phi
      SymbolicHeap(pure2, spatial2, qvars2) <- psi
      combinedVars = qvars ++ qvars2
      // FIXME: Should actually rename the vars in psi where necessary
      if (combinedVars.distinct == combinedVars)
    } yield SymbolicHeap(pure ++ pure2, spatial ++ spatial2, combinedVars)
  }

}
