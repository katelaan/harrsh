package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PointsTo, SymbolicHeap}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by jkatelaa on 10/19/16.
  */
class ReachabilityMatrix private (val underlyingPairs : Set[(Var,Var)]) extends HarrshLogging {

  val reach : mutable.Map[Var,Set[Var]] = mutable.Map.empty.withDefaultValue(Set.empty)

  override def toString = {
    val res = new StringBuilder("MATRIX(\n")
    reach.foreach { case (from, to) =>
      res.appendAll("" + from + " -> " + to.mkString(",") + ",\n")
    }
    res.append(')')
    res.toString
  }

  def isReachable(from : Var, to : Var) : Boolean = reach(from).contains(to)

  def reachableFrom(from: Var): Set[Var] = reach(from)

  def update(from : Var, to : Var, setReachable : Boolean) : Unit = {
    if (setReachable) {
      reach.update(from, reach(from) + to)
    } else {
      reach.update(from, reach(from) - to)
    }
  }

  override def equals(other : Any) ={
    try {
      val otherRM = other.asInstanceOf[ReachabilityMatrix]
      reach == otherRM.reach
    } catch {
      case _:ClassCastException => false
    }
  }

  override def hashCode(): Int = reach.hashCode()

}

object ReachabilityMatrix extends HarrshLogging {

  def emptyMatrix : ReachabilityMatrix = {
    new ReachabilityMatrix(Set.empty)
  }

  def fromPairs(pairs : Seq[(Var,Var)]) : ReachabilityMatrix = {
    val matrix = new ReachabilityMatrix(pairs.toSet)
    for ((from, to) <- pairs) matrix.update(from, to, setReachable = true)
    matrix
  }

  def fromSymbolicHeapAndTrackingInfo(compressedHeap : SymbolicHeap, tracking : TrackingInfo) : ReachabilityMatrix = {

    def ptrToPairs(ptr : PointsTo) : Seq[(Var,Var)] = ptr.to map (trg => (ptr.from, trg))

    val directReachability : Seq[(Var,Var)] = compressedHeap.pointers flatMap ptrToPairs
    val equalities : Set[(Var,Var)] = tracking.equalities.map(atom => (atom.l, atom.r))
    val pairs = reachabilityFixedPoint(compressedHeap, equalities, directReachability.toSet)
    logger.trace("Reached fixed point " + pairs)

    val reach = new ReachabilityMatrix(pairs)
    for {
      (from, to) <- pairs
      if from.isFree && to.isFree
    } {
      reach.update(from, to, setReachable = true)
    }
    logger.trace("Reachability matrix for compressed SH: " + reach)
    reach
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
  def computeExtendedMatrix(ti : TrackingInfo, reachPairs : Set[(Var,Var)], vars : Set[Var]) : ReachabilityMatrix = {
    // TODO Code duplication in matrix computation (plus, we're computing a second matrix on top of the FV-reachability matrix...)
    // Note: Subtract 1, because the null pointer is either explicitly in vars, or to be ignored
    val reach = new ReachabilityMatrix(reachPairs)
    for ((from, to) <- reachPairs) {
      reach.update(from, to, setReachable = true)
    }

    logger.debug(s"Extended matrix: $reach")
    reach
  }
}
