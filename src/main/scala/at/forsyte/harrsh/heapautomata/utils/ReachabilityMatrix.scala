package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.main.{FV, SlexLogging}
import at.forsyte.harrsh.main.FV._
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 10/19/16.
  */
case class ReachabilityMatrix(numFV : Int, reach : Array[Boolean]) extends SlexLogging {

  private val dim = numFV + 1

  if (HeapAutomataSafeModeEnabled) {
    if (reach.length != dim * dim) throw new IllegalStateException("Reachability info is not a " + dim + "*" + dim + " matrix")
  }

  override def toString = "MATRIX(\n" + (for (i <- 0 to numFV) yield getRowFor(i).map(if (_) '1' else '0').mkString(" ")).mkString("\n") + "\n)"

  def isReachable(from : FV, to : FV) : Boolean = isReachableIx(unFV(from), unFV(to))
  def isReachableIx(from : Int, to : Int) : Boolean = {
    val ix = minIndexForSrc(from) + to
    val res = reach(minIndexForSrcIx(from) + to)
    logger.trace("Looking up entry for " + (from, to) + " at index " + ix + " in " + reach.mkString(" ") + " yielding " + res)
    res
  }

  def getRowFor(src: FV): Seq[Boolean] = getRowForIx(unFV(src))
  def getRowForIx(src: Int): Seq[Boolean] = {
    val start = minIndexForSrcIx(src)
    reach.slice(start, start + dim)
  }

  def update(from : FV, to : FV, setReachable : Boolean) : Unit = updateIx(unFV(from), unFV(to), setReachable)
  def updateIx(from : Int, to : Int, setReachable : Boolean) : Unit = {
    val start = minIndexForSrcIx(from)
    reach.update(start + to, setReachable)
    logger.debug("Updating matrix adding " + (from,to,setReachable) + " yielding " + this)
  }

  override def equals(other : Any) = other match {
    case ReachabilityMatrix(_, oreach) => reach.deep == oreach.deep
    case _ => false
  }

  override def hashCode(): Int = reach.deep.hashCode()

  private def minIndexForSrcIx(src : Int) : Int = dim * src
  private def minIndexForSrc(src : FV) : Int = minIndexForSrcIx(unFV(src))

  private def maxIndexForSrcIx(src : Int) : Int = (dim+1) * src - 1
  private def maxIndexForSrc(src : FV) : Int = maxIndexForSrcIx(unFV(src))

}

object ReachabilityMatrix {

  def emptyMatrix(numFV : Int) : ReachabilityMatrix = {
    ReachabilityMatrix(numFV, Array.fill((numFV+1)*(numFV+1))(false))
  }

  def allMatrices(numFV: Int) : Set[ReachabilityMatrix] = {
    val entries = Combinators.allSeqsOfLength((numFV+1) * (numFV+1), Set(false,true))
    entries map (e => ReachabilityMatrix(numFV, e.toArray))
  }

  def fromPairs(numFV : Int, pairs : Seq[(Int,Int)]) : ReachabilityMatrix = {
    val matrix = emptyMatrix(numFV)
    for ((from, to) <- pairs) matrix.update(from, to, setReachable = true)
    matrix
  }
}
