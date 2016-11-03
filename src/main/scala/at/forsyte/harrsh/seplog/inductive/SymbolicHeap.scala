package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.Var
import at.forsyte.harrsh.main.Var._
import at.forsyte.harrsh.heapautomata.HeapAutomataSafeModeEnabled
import at.forsyte.harrsh.seplog.{MapBasedRenaming, Renaming}
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap(pure : Seq[PureAtom], spatial: Seq[SpatialAtom], numFV : Int, qvars : Seq[Var]) {

  // Sanity check
  if (HeapAutomataSafeModeEnabled) {
    val (free, bound) = (pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars)).partition(isFV)
    if (!free.isEmpty && free.max > numFV) throw new IllegalStateException("NumFV = " + numFV + " but contained FVs are " + free.distinct)
  }

  override final def toString = toStringWithVarNames(DefaultNaming)

  def toStringWithVarNames(naming: VarNaming): String = {
    val prefix = qvars map naming map ("\u2203"+_) mkString " "
    val spatialString = spatial.map(_.toStringWithVarNames(naming)).mkString(" * ")
    val pureString = if (pure.isEmpty) "" else pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString + " [" + numFV + "/" + qvars.size + "]"
  }

  def hasPointer: Boolean = spatial.exists(_.isInstanceOf[PointsTo])

  def calledPreds: Seq[String] = spatial filter (_.isInductiveCall) map (_.getPredicateName.get)

  def getCalls : Seq[PredCall] = spatial filter (_.isInductiveCall) map (_.asInstanceOf[PredCall])

  def pointers : Seq[PointsTo] = spatial filter (_.isInstanceOf[PointsTo]) map (_.asInstanceOf[PointsTo])

  def equalities : Seq[PtrEq] = pure filter (_.isInstanceOf[PtrEq]) map (_.asInstanceOf[PtrEq])

  def ptrComparisons : Seq[PureAtom] = pure filter (a => a.isInstanceOf[PtrEq] || a.isInstanceOf[PtrNEq])

  def withoutCalls : SymbolicHeap = copy(spatial = spatial.filter(!_.isInductiveCall))

  def addToCallPreds(tags : Seq[String]) : SymbolicHeap = {
    if (tags.size != getCalls.size) throw new IllegalArgumentException("Wrong number of tags passed")
    val newCalls = getCalls zip tags map {
      case (call,tag) => call.copy(name = call.name + tag)
    }
    val wo = withoutCalls
    wo.copy(spatial = wo.spatial ++ newCalls)
  }

  def renameVars(f : Renaming) = {
    // Rename bound variables if applicable
    val (qvarsRenamed, extendedF) : (Seq[Var], Renaming) = qvars.foldLeft((Seq[Var](), f))({
      case ((seq, intermediateF), v) =>
        val extended = intermediateF.addBoundVarWithOptionalAlphaConversion(v)
        (extended(v) +: seq, extended)
    })

    SymbolicHeap(pure map (_.renameVars(extendedF)), spatial map (_.renameVars(extendedF)), numFV, qvarsRenamed)
  }

  // FIXME Get rid of this method altogether?!
  lazy val getVars : Set[Var] = Set.empty ++ fvars ++ qvars

  lazy val fvars : Seq[Var] =  1 to numFV

}

object SymbolicHeap extends LazyLogging {

  def apply(pure : Seq[PureAtom], spatial: Seq[SpatialAtom]) : SymbolicHeap = {
    val vars = Set.empty ++ pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars)
    val fvars = vars.filter(_ > 0)
    val qvars = vars.filter(_ < 0)

    // TODO: Other sanity checks?
//    if (!fvars.isEmpty && fvars.max != fvars.size) {
//      throw new IllegalStateException("Non-consecutive free variables: " + fvars)
//    } else if (!qvars.isEmpty && qvars.min != (-qvars.size)) {
//      throw new IllegalStateException("Non-consecutive quantified variables: " + qvars)
//    } else {
    //  SymbolicHeap(pure, spatial, fvars.size, qvars.size)
    //}

    // If nothing else is given, we assume the max index gives us the number of free vars
    SymbolicHeap(pure, spatial, if (fvars.isEmpty) 0 else fvars.max, qvars.toSeq)
  }

  def apply(spatial: Seq[SpatialAtom]) : SymbolicHeap = apply(Seq.empty, spatial)

  def combineHeaps(phi : SymbolicHeap, psi : SymbolicHeap) : SymbolicHeap = {
    val SymbolicHeap(pure, spatial, numfv, qvars) = phi

    // Shift the quantified variables in the right SH to avoid name clashes
    val SymbolicHeap(pure2, spatial2, numfv2, qvars2) = psi.renameVars(Renaming.clashAvoidanceRenaming(qvars))

//    logger.debug("Left:    "+phi)
//    logger.debug("Right:   "+psi)
//    logger.debug("Renamed: "+psi.renameVars(Renaming.clashAvoidanceRenaming(qvars)))
//    logger.debug("Result:  "+SymbolicHeap(pure ++ pure2, spatial ++ spatial2, Math.max(numfv, numfv2), qvars ++ qvars2))

    // Free variables remain the same, so we take the maximum
    // Quantified variables are renamed, so we take the sum
    SymbolicHeap(pure ++ pure2, spatial ++ spatial2, Math.max(numfv, numfv2), qvars ++ qvars2)
  }

  def combineAllHeaps(heaps : Seq[SymbolicHeap]) : SymbolicHeap = combineAllHeapsAcc(heaps, empty)

  @tailrec
  private def combineAllHeapsAcc(heaps : Seq[SymbolicHeap], acc : SymbolicHeap) : SymbolicHeap = if (heaps.isEmpty) acc else {
    val comb = combineHeaps(heaps.head, acc)
    combineAllHeapsAcc(heaps.tail, comb)
  }

  val empty = SymbolicHeap(Seq())

}