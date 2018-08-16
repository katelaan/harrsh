package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.util.{Combinators, StringUtils, ToLatex}

import scala.collection.SortedSet

/**
  * Created by jkatelaa on 10/3/16.
  */
case class SymbolicHeap(pure : Seq[PureAtom], pointers: Seq[PointsTo], predCalls : Seq[PredCall], freeVars : Seq[FreeVar]) extends ToStringWithVarnames with HarrshLogging {

  /**
    * Generates a string representation by mapping the (integer) variables to the given string representations
    * @param naming Map from variables to string representations
    * @return String representation of this symbolic heap
    */
  override def toStringWithVarNames(naming: Naming): String = {
    val prefix = (boundVars map naming map ("\u2203"+_)).mkString(" ")
    val spatialString = if (pointers.isEmpty && predCalls.isEmpty) {
      "emp"
    } else {
      (pointers.map(_.toStringWithVarNames(naming)) ++ predCalls.map(_.toStringWithVarNames(naming))).mkString(" * ")
    }
    val pureString = if (pure.isEmpty) "" else pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    prefix + (if (prefix.isEmpty) "" else " . ") + spatialString + pureString //+ " [" + numFV + "/" + boundVars.size + "]"
  }

  lazy val atoms: AtomContainer = AtomContainer(pure, pointers, predCalls)

  lazy val boundVars : SortedSet[BoundVar] = atoms.boundVars

  def isEmpty: Boolean = pure.isEmpty && pointers.isEmpty && predCalls.isEmpty

  def hasPointer: Boolean = pointers.nonEmpty

  def isReduced: Boolean = predCalls.isEmpty

  def nonReduced: Boolean = predCalls.nonEmpty

  val numFV: Int = freeVars.size

  // TODO This should probably not be a method here, but then I have to make sure to cache the value elsewhere to avoid multiple computation
  lazy val freeVariableTrackingInfo : TrackingInfo = TrackingInfo.fromSymbolicHeap(this).projectionToFreeVars

  def alloc: Set[Var] = freeVariableTrackingInfo.alloc

  lazy val identsOfCalledPreds: Seq[String] = predCalls map (_.name)

  lazy val equalities : Seq[PureAtom] = pure filter (_.isEquality)

  lazy val ptrComparisons : Seq[PureAtom] = pure filter (_.isPointerComparison)

  lazy val allVars : Set[Var] = freeVars.toSet ++ boundVars

  def hasVar(v : Var) : Boolean = allVars.contains(v)

  def withoutCalls : SymbolicHeap = copy(predCalls = Seq.empty)

  /**
    * Replaces the predicates calls with the given symbolic heaps, renaming variables as necessary
    * @param shs Instantiations of the symbolic heaps
    * @return
    */
  def replaceCalls(shs : Seq[SymbolicHeap]): SymbolicHeap = {
    if (shs.length != predCalls.length) {
      throw new IllegalArgumentException("Trying to replace " + predCalls.length + " calls with " + shs.length + " symbolic heaps")
    }

    logger.debug(s"My FVs: $freeVars; replacing FVs: ${shs.map(_.freeVars)}")
    logger.debug("Instantiating calls in " + this + " as follows:\n" + (predCalls zip shs).map{
      case (call,instance) => "  " + call + " => " + instance
    }.mkString("\n"))

    val stateHeapPairs = predCalls zip shs
    stateHeapPairs.foldLeft(this){
      case (partiallyInstantiedHeap, (call,instance)) =>
        val intermediate = partiallyInstantiedHeap.replaceCall(call, instance)
        logger.debug(s"After replacing $call: $intermediate")
        intermediate
    }
  }

  /**
    * Replaces a single predicate call with the given symbolic heap, renaming variables as necessary
    * @param call Instantiation of the symbolic heap
    * @return
    */
  def replaceCall(call : PredCall, instance : SymbolicHeap): SymbolicHeap = {
    if (!predCalls.contains(call)) {
      throw new IllegalArgumentException(s"Trying to replace call $call which does not appear in $this")
    }
    logger.debug(s"Will replace call $call with $instance")

    val renamedAtoms = instance.atoms.instantiateFVs(instance.freeVars, call.args)
    val shFiltered = this.copy(predCalls = Combinators.dropFirstMatch[PredCall](predCalls, _ == call))
    logger.debug(s"Renamed atoms in $instance to $renamedAtoms which will be combined with $shFiltered")

    // Shift non-shared variables of the renamed instance to avoid double capture
    val nonShared : Set[BoundVar] = renamedAtoms.boundVars.toSet -- Var.boundVars(call.args.toSet)
    val shiftedAtoms = if (nonShared.isEmpty) {
      logger.trace("No shifting necessary.")
      renamedAtoms
    } else {
      val maxVar = if (boundVars.isEmpty) 0 else boundVars.maxBy(_.index).index
      logger.trace("Will shift " + nonShared.mkString(",") + " because they don't appear in " + call)
      val shifted = renamedAtoms.shiftBoundVars(nonShared, shiftTo = maxVar + 1)
      logger.debug(s"Bound vars shifted to $shifted")
      shifted
    }

    // After the shift, there is no unintentional double capture between bound variables, so we can simply combine the two heaps
    val res = shFiltered ++ shiftedAtoms
    //val res = SymbolicHeap.mergeHeaps(renamedInstance, shFiltered, sharedVars = call.args.map(_.getVarOrZero).toSet)
    logger.debug("Result of combination: " + res)
    res

  }

  /**
    * Receives a sequence of bound variable--free variable pairs, replaces the bound variables with the free variables.
    *
    * Assumes the free variables are not yet used in the heap.
    * If `closeGaps` is true, removes any gaps in the bound var sequence of the resulting heap.
    *
    * @param instantiations Bound variable--free variable pairs, where all free variables are fresh
    * @param closeGaps Close gaps introduced in sequence of free variables?
    * @return Instantiated SH
    */
  def instantiateBoundVars(instantiations : Seq[(BoundVar,Var)], closeGaps : Boolean): SymbolicHeap = {
    // Ensure that new names
    val instantiatingFVs = instantiations.map(_._2)
    assert(instantiatingFVs forall (_.isFree))
    val newFVs = instantiatingFVs.collect {
      case v:FreeVar if !freeVars.contains(v) => v
    }

    val renaming = Renaming.fromPairs(instantiations)
    // Not necessary to avoid double capture, all instantiations are free vars
    val instantiation = atoms.rename(renaming, avoidDoubleCapture = false)

    logger.debug(s"After instantiation: $instantiation")
    val closed = if (closeGaps) {
      val closed = instantiation.closeGapsInBoundVars
      logger.debug(s"After closing gaps: $closed")
      closed
    } else {
      instantiation
    }

    SymbolicHeap(closed, freeVars ++ newFVs)
  }

  /**
    * Return new symbolic heap that additionally contains the given atoms
    * @param other Atoms to add to this SH
    * @return SH with the additional atoms
    */
  def ++(other: AtomContainer): SymbolicHeap = {
    SymbolicHeap(pure ++ other.pure, pointers ++ other.pointers, predCalls ++ other.predCalls, freeVars)
  }

}

object SymbolicHeap extends HarrshLogging {

  val empty = SymbolicHeap()

  private def sortAtoms(atoms : Seq[SepLogAtom]): AtomContainer = {
    atoms.foldLeft(AtomContainer(Seq.empty, Seq.empty, Seq.empty)){
      case (atoms, atom) => atom match {
        case p: PointsTo => atoms.copy(pointers = atoms.pointers :+ p)
        case c: PredCall => atoms.copy(predCalls = atoms.predCalls :+ c)
        case e: PureAtom => atoms.copy(pure = atoms.pure :+ e)
        case _ => throw new Exception("Received unsupported atom type")
      }
    }
  }

  /**
    * SH constructor for when the order of FVs doesn't matter and there are no extra FVs (i.e., no unused parameters).
    * @param atoms
    * @return
    */
  def apply(atoms : SepLogAtom*): SymbolicHeap = {
    val sortedAtoms = sortAtoms(atoms)
    apply(sortedAtoms, sortedAtoms.freeVarSeq)
  }

  /**
    * Construct SH with given predefined sequence of free variables.
    * Use this when the FV order is different from the default (lexicographical) FV order and when there are extra FVs that don't occur in the atoms (i.e., unused parameters)
    * @param fvs
    * @param atoms
    * @return
    */
  def apply(fvs: Seq[FreeVar], atoms : SepLogAtom*): SymbolicHeap = {
    val sortedAtoms = sortAtoms(atoms)
    apply(sortedAtoms, fvs)
  }

  def apply(atoms: AtomContainer, freeVars: Seq[FreeVar]): SymbolicHeap = {
    SymbolicHeap(atoms.pure, atoms.pointers, atoms.predCalls, freeVars)
  }

  def addTagsToPredCalls(sh : SymbolicHeap, tags : Seq[String]) : SymbolicHeap = {
    if (tags.size != sh.predCalls.size) throw new IllegalArgumentException("Wrong number of tags passed")
    val newCalls = sh.predCalls zip tags map {
      case (call,tag) => call.copy(name = call.name + tag)
    }
    sh.copy(predCalls = newCalls)
  }

  /**
    * Serializes the given heap to the Harrsh string format
    * @param sh Heap to serialize
    * @param naming Naming of the variables in the serialization
    * @return
    */
  def toHarrshFormat(sh : SymbolicHeap, naming : Naming) : String = {
    // TODO This is somewhat redundant wrt ordinary string conversion
    val spatial : Seq[ToStringWithVarnames] = sh.pointers ++ sh.predCalls
    val spatialString = if (spatial.isEmpty) "emp" else spatial.map(_.toStringWithVarNames(naming)).mkString(" * ")
    val pureString = if (sh.pure.isEmpty) "" else sh.pure.map(_.toStringWithVarNames(naming)).mkString(" : {", ", ", "}")
    spatialString.replaceAll("\u21a6", "->") ++ pureString.replaceAll("\u2248", "=").replaceAll("\u2249", "!=")
  }

  object ops {

    implicit val symbolicHeapToLatex: ToLatex[SymbolicHeap] = (sh: SymbolicHeap, naming: Naming) => {
      val indexedNaming = Var.Naming.indexify(naming)
      val defaultString = sh.toStringWithVarNames(indexedNaming)
      StringUtils.literalReplacements(Seq(
        "\u2203" -> "\\exists ",
        "." -> "~.~",
        "\u21a6" -> "\\rightarrow",
        "\u2248" -> "=",
        "\u2249" -> "\\neq ",
        NullConst.toString -> "\\nil"
      ), defaultString)
    }

  }

}