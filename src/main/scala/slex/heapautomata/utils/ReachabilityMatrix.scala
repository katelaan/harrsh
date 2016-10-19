package slex.heapautomata.utils

import slex.Combinators
import slex.heapautomata._

/**
  * Created by jkatelaa on 10/19/16.
  */
case class ReachabilityMatrix(numFV : Int, reach : Array[Boolean]) {

  private val dim = numFV + 1

  if (HeapAutomataSafeModeEnabled) {
    //if (reach.length != numFV || reach.exists((_.length != numFV))) throw new IllegalStateException("Reachability info is not a " + numFV + "*" + numFV + " matrix")
    if (reach.length != dim * dim) throw new IllegalStateException("Reachability info is not a " + dim + "*" + dim + " matrix")
  }

  def isReachable(from : Int, to : Int) : Boolean = reach(minIndexForSrc(from) + to)

  def getRowFor(src: FV): Seq[Boolean] = {
    val start = minIndexForSrc(src)
    reach.slice(start, start + dim)
  }

  def update(from : FV, to : FV, setReachable : Boolean) : Unit = update(unFV(from), unFV(to), setReachable)
  def update(from : Int, to : Int, setReachable : Boolean) : Unit = {
    val start = minIndexForSrc(from)
    reach.update(start + to, setReachable)
  }

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
}
