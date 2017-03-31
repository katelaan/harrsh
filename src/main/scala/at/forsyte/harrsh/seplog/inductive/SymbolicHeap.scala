package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.{MapBasedRenaming, PtrExpr, Renaming, Var}
import at.forsyte.harrsh.seplog.Var._

import scala.collection.SortedSet

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap private (pure : Seq[PureAtom], pointers: Seq[PointsTo], predCalls : Seq[PredCall], numFV : Int, boundVars : SortedSet[Var]) extends HarrshLogging {

  // Sanity check
  if (Config.HeapAutomataSafeModeEnabled) {
    val (free, bound) = (pure.flatMap(_.getVars) ++ pointers.flatMap(_.getVars)).partition(isFV)
    if (free.nonEmpty && free.max > numFV) throw new IllegalStateException("NumFV = " + numFV + " but contained FVs are " + free.distinct)
  }

  override final def toString = toStringWithVarNames(DefaultNaming)

  /**
    * Generates a string representation by mapping the (integer) variables to the given string representations
    * @param naming Map from variables to string representations
    * @return String representation of this symbolic heap
    */
  def toStringWithVarNames(naming: VarNaming): String = {
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
    * @param f The renaming function applied to the symbolic heap
    * @param avoidDoubleCapture If the codomain and f contains bound variables of this symbolic heap, they will renamed to avoid double capture iff this parameter is true.
    * @return
    */
  def renameVars(f : Renaming, avoidDoubleCapture : Boolean = true) : SymbolicHeap = {
    logger.info("Renaming vars in " + this)
    logger.debug("Map used for renaming " + f.toString)

    // Rename bound variables if applicable
    val extendedF : Renaming = if (avoidDoubleCapture) {
      boundVars.foldLeft(f)({
        case (intermediateF, v) =>
          intermediateF.addBoundVarWithOptionalAlphaConversion(v)
      })
    } else f
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

  /**
    * Instantiates the free variables of this symbolic heap with the given argument expressions
    * @param args
    * @return
    */
  def instantiateFVs(args : Seq[PtrExpr]): SymbolicHeap = {
    // Rename the free variables of SH to the actual arguments of the predicate calls,
    // i.e. replace the i-th FV with the call argument at index i-1
    val pairs: Seq[(Var, Var)] = ((1 to args.length) map (x => mkVar(x))) zip (args map (_.getVarOrZero))
    val map: Map[Var, Var] = Map() ++ pairs
    renameVars(MapBasedRenaming(map))
  }

  /**
    * Replaces the given bound var with the given free var
    * @param qvar Bound var to instantiate
    * @param instance Free var to use
    * @return Resulting symbolic heap
    */
  def instantiateBoundVarWithFV(qvar : Var, instance : Var) : SymbolicHeap = {
    assert(Var.isBound(qvar))
    if (!Var.isFV(instance)) throw new IllegalArgumentException("Cannot instantiate bound variable by different bound variable")

    val newNumFV = Math.max(numFV, instance)
    val renaming = MapBasedRenaming(Map(qvar -> instance))
    SymbolicHeap(pure map (_.renameVars(renaming)), pointers map (_.renameVars(renaming)), predCalls map (_.renameVars(renaming)), newNumFV, boundVars - qvar)
  }

  /**
    * Replaces the predicates calls with the given symbolic heaps, renaming variables as necessary
    * @param shs Instantiations of the symbolic heaps
    * @param performAlphaConversion Should quantified variables be renamed to avoid double capture? In a regular unfolding, this will be necessary, whereas it may be undesired in the combination of heaps that deliberately share bound variables, e.g. in kernelization or ECD recombination.
    * @return
    */
  def replaceCalls(shs : Seq[SymbolicHeap], performAlphaConversion : Boolean): SymbolicHeap = {
    if (shs.length != predCalls.length) {
      throw new IllegalArgumentException("Trying to replace " + predCalls.length + " calls with " + shs.length + " symbolic heaps")
    }

    logger.debug("Instantiating calls in " + this + " with SHs " + shs.mkString(", "))

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

    val renamedInstance = instance.instantiateFVs(call.args)
    val shFiltered = this.copy(predCalls = predCalls.filterNot(_ == call))
    logger.debug("Renamed " + instance + " to " +renamedInstance + " which will be combined with " + shFiltered)
    val res = SymbolicHeap.mergeHeaps(shFiltered, renamedInstance, sharedVars = call.args.map(_.getVarOrZero).toSet)
    logger.debug("Result of combination: " + res)
    res
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
  def fromFullDescription(pure : Seq[PureAtom], spatial: Seq[PointsTo], calls : Seq[PredCall], numFV : Int, boundVars : Iterable[Var]) = {
    SymbolicHeap(pure, spatial, calls, numFV, SortedSet.empty(boundVarOrdering) ++ boundVars)
  }

  /**
    * Adds the given tags to the given heaps predicate calls (as is necessary in refinement)
    * @param sh Symbolic heap to tag
    * @param tags Tags to add to the calls
    * @return Tagged heap
    */
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
    val SymbolicHeap(pure2, spatial2, calls2, numfv2, qvars2) = psi.renameVars(Renaming.clashAvoidanceRenaming(qvars diff sharedVars))

    // Free variables remain the same, so we take the maximum
    // Quantified variables are only partially renamed, but since we're using sets, duplicates are filtered out automatically
    SymbolicHeap(pure ++ pure2, spatial ++ spatial2, calls ++ calls2, Math.max(numfv, numfv2), qvars ++ qvars2)
  }

  /**
    * Serializes the given heap to the Harrsh string format
    * @param sh Heap to serialize
    * @param naming Naming of the variables in the serialization
    * @return
    */
  def toHarrshFormat(sh : SymbolicHeap, naming : VarNaming) : String = {
    // TODO This is somewhat redundant wrt ordinary string conversion
    val spatialString = sh.pointers.map(_.toStringWithVarNames(naming)).mkString(" * ")
    val pureString = if (sh.pure.isEmpty) "" else sh.pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    spatialString.replaceAll("\u21a6", "->") ++ pureString.replaceAll("\u2248", "=").replaceAll("\u2249", "!=")
  }

}