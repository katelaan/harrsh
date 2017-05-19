package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.main.{Config, HarrshLogging}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.{PointsTo, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/19/16.
  */
case class ReachabilityMatrix(numFV : Int, reach : Array[Boolean], underlyingPairs : Option[Set[(Var,Var)]]) extends HarrshLogging {

  private val dim = numFV + 1

  if (Config.HeapAutomataSafeModeEnabled) {
    if (reach.length != dim * dim) throw new IllegalStateException("Reachability info is not a " + dim + "*" + dim + " matrix")
  }

  override def toString = "MATRIX(\n" + (for (i <- 0 to numFV) yield getRowFor(Var(i)).map(if (_) '1' else '0').mkString(" ")).mkString("\n") + "\n)"

  def isReachable(from : Var, to : Var) : Boolean = isReachableIx(from.toInt, to.toInt)
  def isReachableIx(from : Int, to : Int) : Boolean = {
    val ix = minIndexForSrc(Var(from)) + to
    val res = reach(minIndexForSrcIx(from) + to)
    logger.trace("Looking up entry for " + (from, to) + " at index " + ix + " in " + reach.mkString(" ") + " yielding " + res)
    res
  }

  def getRowFor(src: Var): Seq[Boolean] = getRowForIx(src.toInt)
  def getRowForIx(src: Int): Seq[Boolean] = {
    val start = minIndexForSrcIx(src)
    reach.slice(start, start + dim)
  }

  def update(from : Var, to : Var, setReachable : Boolean) : Unit = updateIx(from.toInt, to.toInt, setReachable)
  def updateIx(from : Int, to : Int, setReachable : Boolean) : Unit = {
    val start = minIndexForSrcIx(from)
    reach.update(start + to, setReachable)
    logger.debug("Updating matrix adding " + (from,to,setReachable) + " yielding " + this)
  }

  override def equals(other : Any) = other match {
    case ReachabilityMatrix(_, oreach, _) => reach.deep == oreach.deep
    case _ => false
  }

  override def hashCode(): Int = reach.deep.hashCode()

  private def minIndexForSrcIx(src : Int) : Int = dim * src
  private def minIndexForSrc(src : Var) : Int = minIndexForSrcIx(src.toInt)

  private def maxIndexForSrcIx(src : Int) : Int = (dim+1) * src - 1
  private def maxIndexForSrc(src : Var) : Int = maxIndexForSrcIx(src.toInt)

}

object ReachabilityMatrix extends HarrshLogging {

  // In an inconsistent state, everything is reachable
  def inconsistentReachabilityMatrix(numFV : Int) = ReachabilityMatrix(numFV, Array.fill((numFV+1)*(numFV+1))(true), Some(Set.empty))

  def emptyMatrix(numFV : Int) : ReachabilityMatrix = {
    ReachabilityMatrix(numFV, Array.fill((numFV+1)*(numFV+1))(false), None)
  }

  def allMatrices(numFV: Int) : Set[ReachabilityMatrix] = {
    val entries = Combinators.allSeqsOfLength((numFV+1) * (numFV+1), Set(false,true))
    entries map (e => ReachabilityMatrix(numFV, e.toArray, None))
  }

  def fromPairs(numFV : Int, pairs : Seq[(Int,Int)]) : ReachabilityMatrix = {
    val matrix = emptyMatrix(numFV)
    for ((from, to) <- pairs) matrix.update(Var(from), Var(to), setReachable = true)
    matrix.copy(underlyingPairs = Some(pairs.map(p => (Var(p._1),Var(p._2))).toSet))
  }

  def fromSymbolicHeapAndTrackingInfo(numFV : Int, compressedHeap : SymbolicHeap, tracking : TrackingInfo) : ReachabilityMatrix = {

    def ptrToPairs(ptr : PointsTo) : Seq[(Var,Var)] = ptr.to map (to => (ptr.fromAsVar, to.getVarOrZero))

    val directReachability : Seq[(Var,Var)] = compressedHeap.pointers flatMap ptrToPairs
    val equalities : Set[(Var,Var)] = tracking.equalities.map(atom => (atom.l.getVarOrZero, atom.r.getVarOrZero))
    val pairs = reachabilityFixedPoint(compressedHeap, equalities, directReachability.toSet)
    logger.trace("Reached fixed point " + pairs)

    val reach = ReachabilityMatrix.emptyMatrix(numFV)
    for {
      (from, to) <- pairs
      if from.isFree && to.isFree
    } {
      reach.update(from, to, setReachable = true)
    }
    logger.trace("Reachability matrix for compressed SH: " + reach)

    reach.copy(underlyingPairs = Some(pairs))
  }

  @tailrec
  private def reachabilityFixedPoint(compressedHeap : SymbolicHeap, equalities: Set[(Var,Var)], pairs : Set[(Var, Var)]) : Set[(Var, Var)] = {

    logger.trace("Iterating reachability computation from " + pairs + " modulo equalities " + equalities)

    // FIXME: Reachability computation is currently extremely inefficient; should replace with a path search algorithm (that regards equalities as steps as well)
    // Propagate equalities
    val transitiveEqualityStep : Set[(Var,Var)] = (for {
      (left, right) <- equalities
      (from, to) <- pairs
      if left == from || left == to || right == from || right == to
    } yield (
      Seq[(Var,Var)]()
        ++ (if (left == from) Seq((right,to)) else Seq())
        ++ (if (right == from) Seq((left,to)) else Seq())
        ++ (if (left == to) Seq((from,right)) else Seq())
        ++ (if (right == to) Seq((from, left)) else Seq()))).flatten
    logger.trace("Equality propagation: " + transitiveEqualityStep)

    // Propagate reachability
    val transitivePointerStep = for {
      (from, to) <- pairs
      (from2, to2) <- pairs
      if to == from2
    } yield (from, to2)
    logger.trace("Pointer propagation: " + transitivePointerStep)

    val newPairs = pairs union transitiveEqualityStep union transitivePointerStep

    if (newPairs == pairs) pairs else reachabilityFixedPoint(compressedHeap, equalities, newPairs)
  }


  /**
    * Computes reachability matrix for the given set of variables (possibly including the nullptr)
    * @param ti Tracking information AFTER congruence closure computation
    * @param reachPairs Reachability between pairs of variables AFTER transitive closure computation
    * @param vars Variables to take into account; add nullptr explicitly to have it included
    * @return (variable-to-matrix-index map, matrix)
    */
  def computeExtendedMatrix(ti : TrackingInfo, reachPairs : Set[(Var,Var)], vars : Set[Var]) : (Map[Var, Int], ReachabilityMatrix) = {
    val ixs : Map[Var, Int] = Map() ++ vars.zipWithIndex

    // TODO Code duplication in matrix computation (plus, we're computing a second matrix on top of the FV-reachability matrix...)
    // Note: Subtract 1, because the null pointer is either explicitly in vars, or to be ignored
    val reach = ReachabilityMatrix.emptyMatrix(vars.size - 1)
    for ((from, to) <- reachPairs) {
      reach.update(Var(ixs(from)), Var(ixs(to)), setReachable = true)
    }

    logger.debug("Extended matrix for variable numbering " + ixs.toSeq.sortBy(_._2).map(p => p._1 + " -> " + p._2).mkString(", ") + ": " + reach)

    (ixs, reach)
  }
}
