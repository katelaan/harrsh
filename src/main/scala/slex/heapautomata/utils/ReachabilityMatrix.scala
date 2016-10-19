package slex.heapautomata.utils

import slex.Combinators
import slex.heapautomata._
import slex.main.SlexLogging

/**
  * Created by jkatelaa on 10/19/16.
  */
case class ReachabilityMatrix(numFV : Int, reach : Array[Boolean]) extends SlexLogging {

  private val dim = numFV + 1

  if (HeapAutomataSafeModeEnabled) {
    if (reach.length != dim * dim) throw new IllegalStateException("Reachability info is not a " + dim + "*" + dim + " matrix")
  }

  override def toString = "MATRIX(\n" + (for (i <- 0 to numFV) yield getRowFor(i).map(if (_) '1' else '0').mkString(" ")).mkString("\n") + "\n)"

  def isReachable(from : FV, to : FV) : Boolean = isReachable(unFV(from), unFV(to))
  def isReachable(from : Int, to : Int) : Boolean = {
    val ix = minIndexForSrc(from) + to
    val res = reach(minIndexForSrc(from) + to)
    logger.debug("Looking up entry for " + (from, to) + " at index " + ix + " in " + reach.mkString(" ") + " yielding " + res)
    res
  }

  def getRowFor(src: FV): Seq[Boolean] = getRowFor(unFV(src))
  def getRowFor(src: Int): Seq[Boolean] = {
    val start = minIndexForSrc(src)
    reach.slice(start, start + dim)
  }

  def update(from : FV, to : FV, setReachable : Boolean) : Unit = update(unFV(from), unFV(to), setReachable)
  def update(from : Int, to : Int, setReachable : Boolean) : Unit = {
    val start = minIndexForSrc(from)
    reach.update(start + to, setReachable)
    logger.debug("Updating matrix adding " + (from,to,setReachable) + " yielding " + this)
  }

  override def equals(other : Any) = other match {
    case ReachabilityMatrix(_, oreach) => reach.deep == oreach.deep
    case _ => false
  }

  override def hashCode(): Int = reach.deep.hashCode()

  private def minIndexForSrc(src : Int) : Int = dim * src
  private def minIndexForSrc(src : FV) : Int = minIndexForSrc(unFV(src))

  private def maxIndexForSrc(src : Int) : Int = (dim+1) * src - 1
  private def maxIndexForSrc(src : FV) : Int = maxIndexForSrc(unFV(src))

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
