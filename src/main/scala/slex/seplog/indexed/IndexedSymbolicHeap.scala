package slex.seplog.indexed

import slex.seplog.Renaming

/**
  * Created by jkatelaa on 10/3/16.
  */
case class IndexedSymbolicHeap(pure : Seq[IndexedPureAtom], spatial: Seq[IndexedSpatialAtom], qvars : Seq[String]) {

  def this(pure : Seq[IndexedPureAtom], spatial: Seq[IndexedSpatialAtom]) = this(pure, spatial, Seq())

  def this(spatial: Seq[IndexedSpatialAtom]) = this(Seq(), spatial, Seq())

  override def toString = {
    val prefix = qvars map ("\u2203"+_) mkString " "
    val spatialString = spatial.mkString(" * ")
    val pureString = if (pure.isEmpty) "" else pure.mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString
  }

  // TODO: Renames both free and bound vars. This might need to change in the future (e.g. alpha conversion)
  def renameVars(f : Renaming) = {
    // Rename bound variables if applicable
    val (qvarsRenamed, extendedF) : (Seq[String], Renaming) = qvars.foldRight((Seq[String](), f))({
      case (v, (seq, intermediateF)) =>
        val extended = intermediateF.addBoundVarWithOptionalAlphaConversion(v)
        (extended(v) +: seq, extended)
    })

    IndexedSymbolicHeap(pure map (_.renameVars(extendedF)), spatial map (_.renameVars(extendedF)), qvarsRenamed)
  }

  def getVars : Set[String] = qvars.toSet ++ pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars)

}

object IndexedSymbolicHeap {

  def apply(pure : Seq[IndexedPureAtom], spatial: Seq[IndexedSpatialAtom]) = new IndexedSymbolicHeap(pure, spatial)

  def apply(spatial: Seq[IndexedSpatialAtom]) = new IndexedSymbolicHeap(spatial)

  def combineHeaps(phi : Option[IndexedSymbolicHeap], psi : Option[IndexedSymbolicHeap]) : Option[IndexedSymbolicHeap] = {
    for {
      IndexedSymbolicHeap(pure, spatial, qvars) <- phi
      right <- psi
      // Rename bound variables in the right formula that clash with the left formula
      IndexedSymbolicHeap(pure2, spatial2, qvars2) = right.renameVars(Renaming.clashAvoidanceRenaming(qvars))
      combinedVars = qvars ++ qvars2
    } yield IndexedSymbolicHeap(pure ++ pure2, spatial ++ spatial2, combinedVars)
  }

  // TODO More efficient implementation, e.g. by a fold
  def combineAllHeaps(heaps : Seq[IndexedSymbolicHeap]) : IndexedSymbolicHeap = if (heaps.isEmpty) IndexedSymbolicHeap(Seq()) else combineHeaps(Some(heaps.head), Some(combineAllHeaps(heaps.tail))).get

}
