package slex.seplog

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap(val pure : Seq[PureAtom], spatial: Seq[SpatialAtom], qvars : Seq[String]) {

  def this(pure : Seq[PureAtom], spatial: Seq[SpatialAtom]) = this(pure, spatial, Seq())

  def this(spatial: Seq[SpatialAtom]) = this(Seq(), spatial, Seq())

  override def toString = {
    val prefix = qvars map ("\u2203"+_) mkString(" ")
    val spatialString = spatial.mkString(" * ")
    val pureString = if (pure.isEmpty) "" else pure.mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString
  }

}

object SymbolicHeap {

  def apply(pure : Seq[PureAtom], spatial: Seq[SpatialAtom]) = new SymbolicHeap(pure, spatial)

  def apply(spatial: Seq[SpatialAtom]) = new SymbolicHeap(spatial)

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
