package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.heapautomata.HeapAutomataSafeModeEnabled
import at.forsyte.harrsh.seplog.{MapBasedRenaming, PtrExpr, Renaming, Var}
import at.forsyte.harrsh.seplog.Var._
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap(pure : Seq[PureAtom], spatial: Seq[SpatialAtom], numFV : Int, boundVars : Seq[Var]) {

  // Sanity check
  if (HeapAutomataSafeModeEnabled) {
    val (free, bound) = (pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars)).partition(isFV)
    if (!free.isEmpty && free.max > numFV) throw new IllegalStateException("NumFV = " + numFV + " but contained FVs are " + free.distinct)
  }

  override final def toString = toStringWithVarNames(DefaultNaming)

  def toStringWithVarNames(naming: VarNaming): String = {
    val prefix = (boundVars map naming map ("\u2203"+_)).sorted.mkString(" ")
    val spatialString = spatial.map(_.toStringWithVarNames(naming)).mkString(" * ")
    val pureString = if (pure.isEmpty) "" else pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString //+ " [" + numFV + "/" + boundVars.size + "]"
  }

  lazy val hasPointer: Boolean = spatial.exists(_.isInstanceOf[PointsTo])

  lazy val identsOfCalledPreds: Seq[String] = spatial filter (_.isInductiveCall) map (_.getPredicateName.get)

  lazy val predCalls : Seq[PredCall] = spatial filter (_.isInductiveCall) map (_.asInstanceOf[PredCall])

  lazy val pointers : Seq[PointsTo] = spatial filter (_.isInstanceOf[PointsTo]) map (_.asInstanceOf[PointsTo])

  lazy val equalities : Seq[PtrEq] = pure filter (_.isInstanceOf[PtrEq]) map (_.asInstanceOf[PtrEq])

  lazy val ptrComparisons : Seq[PureAtom] = pure filter (a => a.isInstanceOf[PtrEq] || a.isInstanceOf[PtrNEq])

  // FIXME Get rid of this method altogether?!
  lazy val allVars : Set[Var] = Set.empty ++ freeVars ++ boundVars

  lazy val freeVars : Seq[Var] =  1 to numFV

  def withoutCalls : SymbolicHeap = copy(spatial = spatial.filter(!_.isInductiveCall))

  def addToCallPreds(tags : Seq[String]) : SymbolicHeap = {
    if (tags.size != predCalls.size) throw new IllegalArgumentException("Wrong number of tags passed")
    val newCalls = predCalls zip tags map {
      case (call,tag) => call.copy(name = call.name + tag)
    }
    val wo = withoutCalls
    wo.copy(spatial = wo.spatial ++ newCalls)
  }

  def renameVars(f : Renaming) = {
    // Rename bound variables if applicable
    val (qvarsRenamed, extendedF) : (Seq[Var], Renaming) = boundVars.foldLeft((Seq[Var](), f))({
      case ((seq, intermediateF), v) =>
        val extended = intermediateF.addBoundVarWithOptionalAlphaConversion(v)
        (extended(v) +: seq, extended)
    })

    SymbolicHeap(pure map (_.renameVars(extendedF)), spatial map (_.renameVars(extendedF)), numFV, qvarsRenamed)
  }

  def instantiateFVs(args : Seq[PtrExpr]): SymbolicHeap = {
    // Rename the free variables of SH to the actual arguments of the predicate calls,
    // i.e. replace the i-th FV with the call argument at index i-1
    val pairs: Seq[(Var, Var)] = ((1 to args.length) map (x => mkVar(x))) zip (args map (_.getVarOrZero))
    val map: Map[Var, Var] = Map() ++ pairs
    renameVars(MapBasedRenaming(map))
  }

  /**
    * Replaces the predicates calls with the given symbolic heaps, renaming variables as necessary
    * @param shs
    */
  def instantiateCalls(shs : Seq[SymbolicHeap]): SymbolicHeap = {
    if (shs.length != predCalls.length) {
      throw new IllegalArgumentException("Trying to replace " + predCalls.length + " calls with " + shs.length + " symbolic heaps")
    }

    val stateHeapPairs = predCalls zip shs
    val renamedHeaps : Seq[SymbolicHeap] = stateHeapPairs map {
      case (call, heap) =>
        heap.instantiateFVs(call.args)
    }
    val shFiltered = this.withoutCalls
    //    logger.debug("Filtered heap: " + shFiltered)
    //    logger.debug("State-heap pairs: " + stateHeapPairs.mkString("\n"))
    //    logger.debug("Renamed heaps:" + renamedHeaps.mkString("\n"))
    val combined = SymbolicHeap.combineAllHeaps(shFiltered +: renamedHeaps)
    combined

  }

}

object SymbolicHeap extends LazyLogging {

  def apply(pure : Seq[PureAtom], spatial: Seq[SpatialAtom]) : SymbolicHeap = {
    val vars = Set.empty ++ pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars)
    val fvars = vars.filter(_ > 0)
    val qvars = vars.filter(_ < 0)

    // If nothing else is given, we assume the max index gives us the number of free vars
    SymbolicHeap(pure, spatial, if (fvars.isEmpty) 0 else fvars.max, qvars.toSeq)
  }

  def apply(spatial: Seq[SpatialAtom]) : SymbolicHeap = apply(Seq.empty, spatial)

  def combineHeaps(phi : SymbolicHeap, psi : SymbolicHeap) : SymbolicHeap = {
    val SymbolicHeap(pure, spatial, numfv, qvars) = phi

    // Shift the quantified variables in the right SH to avoid name clashes
    val SymbolicHeap(pure2, spatial2, numfv2, qvars2) = psi.renameVars(Renaming.clashAvoidanceRenaming(qvars))

    // Free variables remain the same, so we take the maximum
    // Quantified variables are renamed, so we take the sum
    SymbolicHeap(pure ++ pure2, spatial ++ spatial2, Math.max(numfv, numfv2), qvars ++ qvars2)
  }

  def combineAllHeaps(heaps : Seq[SymbolicHeap]) : SymbolicHeap = combineAllHeapsAcc(heaps, empty)

  @tailrec
  private def combineAllHeapsAcc(heaps : Seq[SymbolicHeap], acc : SymbolicHeap) : SymbolicHeap = if (heaps.isEmpty) acc else {
    val comb = combineHeaps(acc, heaps.head)
    combineAllHeapsAcc(heaps.tail, comb)
  }

  val empty = SymbolicHeap(Seq())

}