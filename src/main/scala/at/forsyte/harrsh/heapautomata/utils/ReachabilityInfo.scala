package at.forsyte.harrsh.heapautomata.utils

/**
  * Created by jkatelaa on 3/28/17.
  */
case class ReachabilityInfo(ti : TrackingInfo, rm : ReachabilityMatrix) {

  def dropNonFreeVaraibles = copy(ti = ti.dropNonFreeVariables)

}

object ReachabilityInfo {

  def inconsistentReachabilityInfo(numFV : Int) = ReachabilityInfo(TrackingInfo.inconsistentTrackingInfo(numFV), ReachabilityMatrix.inconsistentReachabilityMatrix(numFV))

}