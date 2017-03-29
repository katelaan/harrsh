package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.BoundedFvAutomatonWithTargetComputation
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.{PtrEq, SymbolicHeap}
import com.typesafe.scalalogging.LazyLogging

/**
  * Created by jkatelaa on 10/18/16.
  * TODO EstablishmentAutomaton as Tagged BaseTrackingAutomaton?
  */
class EstablishmentAutomaton(numFV : Int, acceptEstablished : Boolean) extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import at.forsyte.harrsh.heapautomata.instances.BaseTrackingAutomaton._

  override val description = (if (acceptEstablished) "EST_" else "NONEST_") + numFV

  override type State = (TrackingInfo, Boolean)

  override lazy val states: Set[State] = computeTrackingStateSpace(numFV) flatMap (s => Set((s,false), (s,true)))

  override def isFinal(s: State): Boolean = s._2 == acceptEstablished

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {

    val trackingInfo = src map (_._1)
    // All src states are established iff there is no src state of the form (_,false)
    val allSrcsEstablished = !(src exists (!_._2))

    // Get compression + propagation including bound variables
    val inconsistent = TrackingInfo.inconsistentTrackingInfo(numFV)
    val trackingTargetWithoutCleanup = BaseTrackingAutomaton.compressAndPropagateTracking(trackingInfo, lab, inconsistent)

    // Unless we already know that one of the children is not established,
    // check whether everything in that heap is either allocated or equal to a free variable
    // FIXME Is an inconsistent heap established? Currently we handle it like that
    // TODO Get rid of the state comparison (by returning consistency bit or something?)
    val establishmentBit = (trackingTargetWithoutCleanup == inconsistent) ||
        (if (!allSrcsEstablished) {
          false
        } else {
          val allVars = lab.allVars
          logger.debug("Checking establishment of " + allVars.mkString(", "))
          !allVars.exists(!EstablishmentAutomaton.isEstablished(trackingTargetWithoutCleanup, _))
        })

    // Drop the bound variables to get the resulting state of the tracking automaton
    val trackingTarget = trackingTargetWithoutCleanup.dropNonFreeVariables
    logger.debug("Reached state (" + trackingTarget + "," + establishmentBit + ")")

    Set((trackingTarget, establishmentBit))
  }

  // TODO Not needed?!
  override val InconsistentState: (TrackingInfo, Boolean) = (TrackingInfo.inconsistentTrackingInfo(numFV), !acceptEstablished)
}

object EstablishmentAutomaton extends LazyLogging {

  /**
    * Is variable v established according to tracking info s?
    */
  def isEstablished(s : TrackingInfo, v : Var) = {
    isFV(v) || s.alloc.contains(v) || s.pure.exists({
      // Return true iff the pure atom witnesses that v is equal to a free variable
      // This is enough to show establishment, because we assume that s is congruence closed
      case PtrEq(l, r) => (l.getVarOrZero == v && isFV(r.getVarOrZero)) || (r.getVarOrZero == v && isFV(l.getVarOrZero))
      case _ => false
    })
  }

}
