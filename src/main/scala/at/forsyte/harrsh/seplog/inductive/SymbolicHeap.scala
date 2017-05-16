package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec
import scala.collection.SortedSet

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap private (pure : Seq[PureAtom], pointers: Seq[PointsTo], predCalls : Seq[PredCall], numFV : Int, boundVars : SortedSet[Var]) extends ToStringWithVarnames with HarrshLogging {

  // Sanity check
  if (Config.HeapAutomataSafeModeEnabled) {
    val (free, bound) = (pure.flatMap(_.getVars) ++ pointers.flatMap(_.getVars)).partition(isFV)
    if (free.nonEmpty && free.max > numFV) throw new IllegalStateException("NumFV = " + numFV + " but contained FVs are " + free.distinct)
  }

  /**
    * Generates a string representation by mapping the (integer) variables to the given string representations
    * @param naming Map from variables to string representations
    * @return String representation of this symbolic heap
    */
  override def toStringWithVarNames(naming: VarNaming): String = {
    val prefix = (boundVars map naming map ("\u2203"+_)).mkString(" ")
    val spatialString = if (pointers.isEmpty && predCalls.isEmpty) {
      "emp"
    } else {
      (pointers.map(_.toStringWithVarNames(naming)) ++ predCalls.map(_.toStringWithVarNames(naming))).mkString(" * ")
    }
    val pureString = if (pure.isEmpty) "" else pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString //+ " [" + numFV + "/" + boundVars.size + "]"
  }

  def hasPointer: Boolean = pointers.nonEmpty

  def hasPredCalls: Boolean = predCalls.nonEmpty

  lazy val identsOfCalledPreds: Seq[String] = predCalls map (_.name)

  lazy val equalities : Seq[PtrEq] = pure filter (_.isInstanceOf[PtrEq]) map (_.asInstanceOf[PtrEq])

  lazy val ptrComparisons : Seq[PureAtom] = pure filter (_.isPointerComparison)

  lazy val allVars : Set[Var] = freeVars.toSet ++ boundVars

  def hasVar(v : Var) : Boolean = (1 <= v && v <= numFV) || boundVars(v)

  lazy val freeVars : Seq[Var] =  1 to numFV

  def withoutCalls : SymbolicHeap = copy(predCalls = Seq.empty)

  /**
    * Returns new symbolic heap whose variables have been renamed based on the given renaming.
    * In unfolding, you will always want to avoid double capture, whereas certain transformations/rewritings might identify bound variables on purpose, e.g. to remove redundant variables (cf. [[at.forsyte.harrsh.pure.EqualityBasedSimplifications]])
    * Note that this method does NOT close gaps in the sequence of bound variables that it might introduce. To avoid gaps, use [[instantiateBoundVarsWithFVs()]]
    * @param f The renaming function applied to the symbolic heap
    * @param avoidDoubleCapture If the codomain and f contains bound variables of this symbolic heap, they will renamed to avoid double capture iff this parameter is true.
    * @return
    */
  def renameVars(f : Renaming, avoidDoubleCapture : Boolean = true) : SymbolicHeap = {
    logger.info("Renaming vars in " + this)

    // Rename bound variables if applicable
    val extendedF : Renaming = if (avoidDoubleCapture) {
      // Drop keys that never apply so as to avoid spurious renaming of bound Vars
      // Note: In case a free variable does not occur in the heap but DOES occur in the set of free variables (i.e., if there are gaps in the used FVs), this fails
      val tightF = f/*.filter{
        case (k,v) => this.allVars.contains(k)
      }*/

      boundVars.foldLeft(tightF)({
        case (intermediateF, v) =>
          if (!f.isDefinedAt(v)) intermediateF.addBoundVarWithOptionalAlphaConversion(v) else intermediateF
          //intermediateF.addBoundVarWithOptionalAlphaConversion(v)
      })
    } else f
    logger.debug("Map for renaming " + f.toString + " extended to " + extendedF.toString)

    val pureRenamed = pure map (_.renameVars(extendedF))
    val ptrsRenamed = pointers map (_.renameVars(extendedF))
    val callsRenamed = predCalls map (_.renameVars(extendedF))
    // Have the constructor figure out the new number of FVs + the new set of qvars
    val res = SymbolicHeap(pureRenamed, ptrsRenamed, callsRenamed)
    logger.debug("After renaming: " + res)

    // Detect gap in bound vars
    // TODO Do we perhaps want to always close caps in bound vars? See also TODO in [[SymbolicHeapTest]]
//    if (res.boundVars.nonEmpty && res.boundVars.size != -res.boundVars.min) {
//      println("Gap in bound vars: Max bound var " + (-res.boundVars.min) + ", #bound vars " + res.boundVars.size + " (bound vars: " + res.boundVars.mkString(", ") + " in " + res + ")")
//    }

    res
  }

  /**
    * Receives a sequence of bound variable--free variable pairs, replaces the bound variables with the free variables and if the second parameter is true removes any gaps in the bound var sequence of the resulting heap.
    * @param instantiations Bound variable--free variable pairs
    * @param closeGaps Close gaps introduced in sequence of free variables?
    * @return Instantiated SH
    */
  def instantiateBoundVars(instantiations : Seq[(Var,Var)], closeGaps : Boolean): SymbolicHeap = {
    for { (qvar,instance) <- instantiations } {
      assert(Var.isBound(qvar))
      if (!Var.isFV(instance)) throw new IllegalArgumentException("Cannot instantiate bound variable " + Var.toDefaultString(qvar) + " by different bound variable" + Var.toDefaultString(instance))
    }

    val renaming = Renaming.fromPairs(instantiations)
    // Not necessary to avoid double capture, all instantiations are free vars
    val instantiation = renameVars(renaming, avoidDoubleCapture = false)

    logger.debug("After instantiation: " + instantiation)

    // Detect gap in bound vars + remove it
    if (closeGaps) instantiation.closeGapsInBoundVars else instantiation
  }

  /**
    * Closes the gaps in the sequence of bound variables (if any) by renaming bound vars
    * @return
    */
  def closeGapsInBoundVars(): SymbolicHeap = {
    if (boundVars.nonEmpty && boundVars.size != -boundVars.min) {
      logger.trace("Gap in bound vars: Max bound var " + (-boundVars.min) + ", #bound vars " + boundVars.size + " (bound vars: " + boundVars.mkString(", ") + " in " + this + ")")
      val renamingPairs: Seq[(Var, Var)] = boundVars.toSeq.zipWithIndex.map {
        pair => (pair._1, -pair._2 - 1)
      }
      logger.trace("Will close gaps by renaming " + renamingPairs.mkString(", "))
      val renaming = Renaming.fromPairs(renamingPairs)
      val res = renameVars(renaming, avoidDoubleCapture = false)
      logger.debug("Closed gaps in " + this + " yielding: " + res)
      res
    } else {
      this
    }
  }

  /**
    * Replaces the predicates calls with the given symbolic heaps, renaming variables as necessary
    * @param shs Instantiations of the symbolic heaps
    * @return
    */
  def replaceCalls(shs : Seq[SymbolicHeap]): SymbolicHeap = {
    if (shs.length != predCalls.length) {
      throw new IllegalArgumentException("Trying to replace " + predCalls.length + " calls with " + shs.length + " symbolic heaps")
    }

    logger.debug("Instantiating calls in " + this + " as follows:\n" + (predCalls zip shs).map{
      case (call,instance) => "  " + call + " => " + instance
    }.mkString("\n"))

    val stateHeapPairs = predCalls zip shs
    stateHeapPairs.foldLeft(this){
      case (partiallyInstantiedHeap, (call,instance)) => partiallyInstantiedHeap.replaceCall(call, instance)
    }
  }

  /**
    * Replaces a single predicate call with the given symbolic heap, renaming variables as necessary
    * @param call Instantiation of the symbolic heap
    * @return
    */
  def replaceCall(call : PredCall, instance : SymbolicHeap): SymbolicHeap = {
    if (!predCalls.contains(call)) {
      throw new IllegalArgumentException("Trying to replace call " + call + " which does not appear in " + this)
    }
    logger.debug("Will replace call " + call + " with " + instance)

    val renamedInstance = instance.instantiateFVs(call.args)
    val shFiltered = this.copy(predCalls = Combinators.dropFirstMatch[PredCall](predCalls, _ == call))
    logger.debug("Renamed " + instance + " to " +renamedInstance + " which will be combined with " + shFiltered)

    // Shift non-shared variables of the renamed instance to avoid double capture
    val nonShared = renamedInstance.boundVars.toSet -- call.args.map(_.getVarOrZero)
    val shifted = if (nonShared.isEmpty) {
      logger.trace("No shifting necessary.")
      renamedInstance
    } else {
      val maxVar = if (boundVars.isEmpty) 0 else -boundVars.min
      logger.trace("Will shift " + nonShared.map(Var.toDefaultString).mkString(",") + " because they don't appear in " + call)
      val shifted = renamedInstance.shiftBoundVars(nonShared, shiftTo = maxVar + 1)
      logger.debug(renamedInstance + " shifted to " + shifted)
      shifted
    }

    // After the shift, there is no unintentional double capture between bound variables, so we can simply combine the two heaps
    val res = SymbolicHeap(shFiltered.pure ++ shifted.pure, shFiltered.pointers ++ shifted.pointers, shFiltered.predCalls ++ shifted.predCalls)
    //val res = SymbolicHeap.mergeHeaps(renamedInstance, shFiltered, sharedVars = call.args.map(_.getVarOrZero).toSet)
    logger.debug("Result of combination: " + res)
    res

  }

  /**
    * Instantiates the free variables of this symbolic heap with the given argument expressions (e.g. to obtain a heap to replace a call)
    * @param args
    * @return
    */
  private def instantiateFVs(args : Seq[PtrExpr]): SymbolicHeap = {
    // Rename the free variables of SH to the actual arguments of the predicate calls,
    // i.e. replace the i-th FV with the call argument at index i-1
    val pairs: Seq[(Var, Var)] = ((1 to args.length) map (x => mkVar(x))) zip (args map (_.getVarOrZero))
    renameVars(Renaming.fromPairs(pairs))
  }

  private def shiftBoundVars(vars : Set[Var], shiftTo : Var) : SymbolicHeap = {
    //assert(boundVars.size == -boundVars.min
    val pairs = vars.zipWithIndex.map(pair => (pair._1, -pair._2-shiftTo))
    logger.trace("Will execute shifting to " + shiftTo + " => using pairs " + pairs.map(pair => (Var.toDefaultString(pair._1), Var.toDefaultString(pair._2))).mkString(","))
    renameVars(Renaming.fromPairs(pairs), avoidDoubleCapture = false)
  }

}

object SymbolicHeap extends HarrshLogging {

  /**
    * The ordering used for bound variable sets. Use reverse ordering, because bound vars "grow" towards minus infinity.
    */
  private val boundVarOrdering = Ordering.fromLessThan[Var](_ > _)

  val empty = SymbolicHeap(Seq())

  /**
    * The standard constructor for symbolic heaps: Cosntructs heap from pointers, calls and pure constraints.
    * @param pure Pure constraints
    * @param spatial Pointers
    * @param calls Predicate calls
    * @return
    */
  def apply(pure : Seq[PureAtom], spatial: Seq[PointsTo], calls : Seq[PredCall]) : SymbolicHeap = {
    val vars = SortedSet.empty(boundVarOrdering) ++ pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars) ++ calls.flatMap(_.getVars)
    val fvars = vars.filter(_ > 0)
    val qvars = vars.filter(_ < 0)

    // If nothing else is given, we assume the max index gives us the number of free vars
    SymbolicHeap(pure, spatial, calls, if (fvars.isEmpty) 0 else fvars.max, qvars)
  }

  /**
    * Constructs symbolic heap without any pure constraints
    * @param spatial Pointers
    * @param calls Predicate calls
    * @return
    */
  def apply(spatial: Seq[PointsTo], calls: Seq[PredCall]) : SymbolicHeap = apply(Seq.empty, spatial, calls)

  /**
    * Constructs symbolic heap without calls or pure constraints.
    * @param spatial Pointers
    * @return
    */
  def apply(spatial: Seq[PointsTo]) : SymbolicHeap = apply(Seq.empty, spatial, Seq.empty)

  /**
    * In the rare case that you want to override the automatic computation of free variables and bound variables as per
    * the default constructor, this method allows you to construct a symbolic heap from explicit info about free/bound
    * vars. Consider calling the three-parameter apply method instead.
    */
  def fromFullDescription(pure : Seq[PureAtom], spatial: Seq[PointsTo], calls : Seq[PredCall], numFV : Int, boundVars : Iterable[Var]): SymbolicHeap = {
    SymbolicHeap(pure, spatial, calls, numFV, SortedSet.empty(boundVarOrdering) ++ boundVars)
  }

//  def addTagToPredCall(sh : SymbolicHeap, call : PredCall, tag : String) : SymbolicHeap = {
//    if (!sh.predCalls.contains(call)) throw new IllegalArgumentException("Non-existent call passed")
//    sh.copy(predCalls = sh.predCalls.updated(sh.predCalls.indexOf(call), call.copy(name = call.name + tag)))
//  }

  def addTagsToPredCalls(sh : SymbolicHeap, tags : Seq[String]) : SymbolicHeap = {
    if (tags.size != sh.predCalls.size) throw new IllegalArgumentException("Wrong number of tags passed")
    val newCalls = sh.predCalls zip tags map {
      case (call,tag) => call.copy(name = call.name + tag)
    }
    sh.copy(predCalls = newCalls)
  }

  /**
    * Combines the two given heaps into a single, larger heap, avoding name clashes of bound variables that are not in the sequence of shared variables
    * @param phi First heap
    * @param psi Second heap
    * @param sharedVars Shared quantified variables that are not to be renamed by alpha conversion
    * @return Merged heap
    */
  def mergeHeaps(phi : SymbolicHeap, psi : SymbolicHeap, sharedVars : Set[Var]) : SymbolicHeap = {

    val SymbolicHeap(pure, spatial, calls, numfv, qvars) = phi

    // Shift the quantified variables in the right SH to avoid name clashes
    val psiRen@SymbolicHeap(pure2, spatial2, calls2, numfv2, qvars2) = psi.renameVars(Renaming.clashAvoidanceRenaming(qvars diff sharedVars))

    logger.debug("Renaming " + psi + " to " + psiRen + " (avoiding name clashes with " + qvars +  " but ignoring clasehs with " + sharedVars + ")")

    // Free variables remain the same, so we take the maximum
    // Quantified variables are only partially renamed, but since we're using sets, duplicates are filtered out automatically
    SymbolicHeap(pure ++ pure2, spatial ++ spatial2, calls ++ calls2, Math.max(numfv, numfv2), qvars ++ qvars2)
  }

  def dropQuantifiers(sh: SymbolicHeap): SymbolicHeap = {
    val newFreeVars = sh.numFV + 1 until sh.numFV + 1 + sh.boundVars.size
    val instantiations = sh.boundVars.toSeq zip newFreeVars
    sh.instantiateBoundVars(instantiations, closeGaps = false)
  }

  /**
    * Serializes the given heap to the Harrsh string format
    * @param sh Heap to serialize
    * @param naming Naming of the variables in the serialization
    * @return
    */
  def toHarrshFormat(sh : SymbolicHeap, naming : VarNaming) : String = {
    // TODO This is somewhat redundant wrt ordinary string conversion
    val spatial : Seq[ToStringWithVarnames] = sh.pointers ++ sh.predCalls
    val spatialString = if (spatial.isEmpty) "emp" else spatial.map(_.toStringWithVarNames(naming)).mkString(" * ")
    val pureString = if (sh.pure.isEmpty) "" else sh.pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    spatialString.replaceAll("\u21a6", "->") ++ pureString.replaceAll("\u2248", "=").replaceAll("\u2249", "!=")
  }

  def toLatex(sh: SymbolicHeap, naming : VarNaming = DefaultNaming) = {
    val defaultSting = sh.toStringWithVarNames(naming)
    val withMacros = literalReplacements(Seq(
      "\u2203" -> "\\exists ",
      "." -> "~.~",
      "\u21a6" -> "->",
      "\u2248"-> "=",
      "\u2249" -> "\\neq ",
      NullPtr().toString -> "\\nil"
    ), defaultSting)
    indexifyNumbers(withMacros)
  }

  @tailrec private def literalReplacements(reps : Seq[(String,String)], s : String) : String = {
    if (reps.isEmpty) s else literalReplacements(reps.tail, s.replaceAllLiterally(reps.head._1, reps.head._2))
  }

  private def indexifyNumbers(s : String) : String = {
    s.foldLeft[(String,Char)](("",'?')){
      case ((s,prev), curr) => (prev.isDigit, curr.isDigit) match {
        case (false, true) => (s + "_{" + curr, curr)
        case (true, false) => (s + "}" + curr, curr)
        case _ => (s + curr, curr)
      }
    }._1
  }

}