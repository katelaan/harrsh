package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.{MapBasedRenaming, PtrExpr, Renaming, Var}
import at.forsyte.harrsh.seplog.Var._

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap(pure : Seq[PureAtom], pointers: Seq[PointsTo], predCalls : Seq[PredCall], numFV : Int, boundVars : Seq[Var]) extends HarrshLogging {

  // Sanity check
  if (Config.HeapAutomataSafeModeEnabled) {
    val (free, bound) = (pure.flatMap(_.getVars) ++ pointers.flatMap(_.getVars)).partition(isFV)
    if (!free.isEmpty && free.max > numFV) throw new IllegalStateException("NumFV = " + numFV + " but contained FVs are " + free.distinct)
  }

  override final def toString = toStringWithVarNames(DefaultNaming)

  def toStringWithVarNames(naming: VarNaming): String = {
    val prefix = (boundVars map naming map ("\u2203"+_)).sorted.mkString(" ")
    val spatialString = if (pointers.isEmpty && predCalls.isEmpty) {
      "emp"
    } else {
      (pointers.map(_.toStringWithVarNames(naming)) ++ predCalls.map(_.toStringWithVarNames(naming))).mkString(" * ")
    }
    val pureString = if (pure.isEmpty) "" else pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString //+ " [" + numFV + "/" + boundVars.size + "]"
  }

  lazy val hasPointer: Boolean = pointers.nonEmpty

  def hasPredCalls: Boolean = predCalls.nonEmpty

  lazy val identsOfCalledPreds: Seq[String] = predCalls map (_.name)

  lazy val equalities : Seq[PtrEq] = pure filter (_.isInstanceOf[PtrEq]) map (_.asInstanceOf[PtrEq])

  lazy val ptrComparisons : Seq[PureAtom] = pure filter (a => a.isInstanceOf[PtrEq] || a.isInstanceOf[PtrNEq])

  // FIXME Get rid of this method altogether?!
  lazy val allVars : Set[Var] = Set.empty ++ freeVars ++ boundVars

  lazy val freeVars : Seq[Var] =  1 to numFV

  def withoutCalls : SymbolicHeap = copy(predCalls = Seq.empty)

  def addToCallPreds(tags : Seq[String]) : SymbolicHeap = {
    if (tags.size != predCalls.size) throw new IllegalArgumentException("Wrong number of tags passed")
    val newCalls = predCalls zip tags map {
      case (call,tag) => call.copy(name = call.name + tag)
    }
    copy(predCalls = newCalls)
  }

  def renameVars(f : Renaming) = {
    logger.info("Renaming vars in " + this)
    logger.debug("Map used for renaming " + f.toString)

    // Rename bound variables if applicable
    val extendedF : Renaming = boundVars.foldLeft(f)({
      case (intermediateF, v) =>
        intermediateF.addBoundVarWithOptionalAlphaConversion(v)
    })
    logger.debug("Extended map used for renaming" + extendedF.toString)

    val pureRenamed = pure map (_.renameVars(extendedF))
    val ptrsRenamed = pointers map (_.renameVars(extendedF))
    val callsRenamed = predCalls map (_.renameVars(extendedF))
    logger.debug("Pure = " + pureRenamed.mkString(", ") + "; Pointers = " + ptrsRenamed.mkString(", ") + "; Calls = " + callsRenamed.mkString(", "))
    // Have the constructor figure out the new number of FVs + the new set of qvars
    val res = SymbolicHeap(pureRenamed, ptrsRenamed, callsRenamed)
    logger.debug("After renaming: " + res)
    res
  }

//  def renameVars(f : Renaming) = {
//    // Rename bound variables if applicable
//    val (qvarsRenamed, extendedF) : (Seq[Var], Renaming) = boundVars.foldLeft((Seq[Var](), f))({
//      case ((seq, intermediateF), v) =>
//        val extended = intermediateF.addBoundVarWithOptionalAlphaConversion(v)
//        (extended(v) +: seq, extended)
//    })
//
//    val pureRenamed = pure map (_.renameVars(extendedF))
//    val ptrsRenamed = pointers map (_.renameVars(extendedF))
//    val callsRenamed = predCalls map (_.renameVars(extendedF))
//    SymbolicHeap(pureRenamed, ptrsRenamed, callsRenamed, numFV, qvarsRenamed)
//  }

  /**
    * In addition to just renaming vars, this variant also introduces new quantifiers if there are new bound vars in the codomain of the renaming
    * @param f
    * @return
    */
  def renameVarsWithAdditionalQuantification(f : Renaming) = {
    // Rename bound variables if applicable
    val (qvarsRenamed, extendedF) : (Seq[Var], Renaming) = boundVars.foldLeft((Seq[Var](), f))({
      case ((seq, intermediateF), v) =>
        val extended = intermediateF.addBoundVarWithOptionalAlphaConversion(v)
        (extended(v) +: seq, extended)
    })

    val newQVars = extendedF.codomain.filter(Var.isBound).filterNot(qvarsRenamed.contains)
    val allQVars = (qvarsRenamed ++ newQVars).sortWith(_>_)
    SymbolicHeap(pure map (_.renameVars(extendedF)), pointers map (_.renameVars(extendedF)), predCalls map (_.renameVars(extendedF)), numFV, allQVars)
  }

  def instantiateBoundVar(qvar : Var, instance : Var) : SymbolicHeap = {
    if (!Var.isFV(instance)) throw new IllegalArgumentException("Cannot instantiate bound variable by different bound variable")

    val newNumFV = Math.max(numFV, instance)
    val renaming = MapBasedRenaming(Map(qvar -> instance))
    SymbolicHeap(pure map (_.renameVars(renaming)), pointers map (_.renameVars(renaming)), predCalls map (_.renameVars(renaming)), newNumFV, boundVars filterNot (_ == qvar))
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
  def replaceCalls(shs : Seq[SymbolicHeap], performAlphaConversion : Boolean): SymbolicHeap = {
    if (shs.length != predCalls.length) {
      throw new IllegalArgumentException("Trying to replace " + predCalls.length + " calls with " + shs.length + " symbolic heaps")
    }

    logger.debug("Instantiating calls in " + this + " with SHs " + shs.mkString(", "))
    val stateHeapPairs = predCalls zip shs
    val renamedHeaps : Seq[SymbolicHeap] = stateHeapPairs map {
      case (call, heap) =>
        val res = heap.instantiateFVs(call.args)
        logger.debug("Unfolding call " + call + ": Instantiating vars in " + heap + " with " + call.args.mkString("(",",",")") + " yielding " + res)
        res
    }
    val shFiltered = this.withoutCalls
        logger.debug("Filtered heap: " + shFiltered)
        logger.debug("State-heap pairs: " + stateHeapPairs.mkString("\n"))
        logger.debug("Renamed heaps:" + renamedHeaps.mkString("\n"))
    val combined = SymbolicHeap.combineAllHeaps(shFiltered +: renamedHeaps, performAlphaConversion)
    combined

  }

  def replaceCall(call : PredCall, instance : SymbolicHeap, performAlphaConversion : Boolean): SymbolicHeap = {
    if (!predCalls.contains(call)) {
      throw new IllegalArgumentException("Trying to replace call " + call + " which does not appear in " + this)
    }

    val renamedInstance = instance.instantiateFVs(call.args)
    val shFiltered = this.copy(predCalls = predCalls.filterNot(_ == call))
    SymbolicHeap.combineHeaps(shFiltered, renamedInstance, performAlphaConversion)
  }

}

object SymbolicHeap extends HarrshLogging {

  def apply(pure : Seq[PureAtom], spatial: Seq[PointsTo], calls : Seq[PredCall]) : SymbolicHeap = {
    val vars = Set.empty ++ pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars) ++ calls.flatMap(_.getVars)
    val fvars = vars.filter(_ > 0)
    val qvars = vars.filter(_ < 0)

    // If nothing else is given, we assume the max index gives us the number of free vars
    SymbolicHeap(pure, spatial, calls, if (fvars.isEmpty) 0 else fvars.max, qvars.toSeq)
  }

  def apply(spatial: Seq[PointsTo], calls: Seq[PredCall]) : SymbolicHeap = apply(Seq.empty, spatial, calls)

  def apply(spatial: Seq[PointsTo]) : SymbolicHeap = apply(Seq.empty, spatial, Seq.empty)

  def combineHeaps(phi : SymbolicHeap, psi : SymbolicHeap, performAlphaConversion : Boolean) : SymbolicHeap = {
    if (performAlphaConversion)
      combineHeapsWithAlphaConversion(phi, psi)
    else
      combineHeapsWithoutAlphaConversion(phi, psi)
  }

  private def combineHeapsWithAlphaConversion(phi : SymbolicHeap, psi : SymbolicHeap) : SymbolicHeap = {
    val SymbolicHeap(pure, spatial, calls, numfv, qvars) = phi

    // Shift the quantified variables in the right SH to avoid name clashes
    val SymbolicHeap(pure2, spatial2, calls2, numfv2, qvars2) = psi.renameVars(Renaming.clashAvoidanceRenaming(qvars))

    // Free variables remain the same, so we take the maximum
    // Quantified variables are renamed, so we take the sum
    SymbolicHeap(pure ++ pure2, spatial ++ spatial2, calls ++ calls2, Math.max(numfv, numfv2), qvars ++ qvars2)
  }

  private def combineHeapsWithoutAlphaConversion(phi : SymbolicHeap, psi : SymbolicHeap) : SymbolicHeap = {
    val SymbolicHeap(pure, spatial, calls, numfv, qvars) = phi
    val SymbolicHeap(pure2, spatial2, calls2, numfv2, qvars2) = psi

    // Free variables remain the same, so we take the maximum
    // Quantified variables are partially identified, so we have to filter out the duplicates
    SymbolicHeap(pure ++ pure2, spatial ++ spatial2, calls ++ calls2, Math.max(numfv, numfv2), qvars ++ qvars2.filterNot(qvars.contains))
  }

  def combineAllHeaps(heaps : Seq[SymbolicHeap], performAlphaConversion : Boolean) : SymbolicHeap ={
    @tailrec def combineAllHeapsAcc(heaps : Seq[SymbolicHeap], acc : SymbolicHeap) : SymbolicHeap = if (heaps.isEmpty) acc else {
      val comb = combineHeaps(acc, heaps.head, performAlphaConversion)
      combineAllHeapsAcc(heaps.tail, comb)
    }

    logger.debug("Will cobmine all heaps:\n " + heaps.map(" - " + _).mkString("\n"))
    combineAllHeapsAcc(heaps, empty)
  }

  val empty = SymbolicHeap(Seq())

  def toHarrshFormat(sh : SymbolicHeap, naming : VarNaming) : String = {
    // TODO This is somewhat redundant wrt ordinary string conversion
    val spatialString = sh.pointers.map(_.toStringWithVarNames(naming)).mkString(" * ")
    val pureString = if (sh.pure.isEmpty) "" else sh.pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    spatialString.replaceAll("\u21a6", "->") ++ pureString.replaceAll("\u2248", "=").replaceAll("\u2249", "!=")
  }

}