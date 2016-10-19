package slex.heapautomata

import slex.seplog.{PtrEq, SymbolicHeap}

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomaton(numFV : Int, acceptEstablished : Boolean) extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import BaseTrackingAutomaton._

  override val description = (if (acceptEstablished) "EST_" else "NONEST_") + numFV

  override type State = (TrackingInfo, Boolean)

  override lazy val states: Set[State] = computeTrackingStateSpace(numFV) flatMap (s => Set((s,false), (s,true)))

  override def isFinal(s: State): Boolean = s._2 == acceptEstablished

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {

    val trackingInfo = src map (_._1)
    // All src states are established iff there is no src state of the form (_,false)
    val allSrcsEstablished = !(src exists (!_._2))

    // Get compression + propagation including bound variables
    val inconsistent = inconsistentTrackingInfo(numFV)
    val trackingTargetWithoutCleanup = compressAndPropagate(trackingInfo, lab, inconsistentTrackingInfo(numFV))

    // Unless we already know that one of the children is not established,
    // check whether everything in that heap is either allocated or equal to a free variable
    // FIXME Is an inconsistent heap established? Currently we handle it like that
    // TODO Get rid of the state comparison (by returning consistency bit or something?)
    val establishmentBit = (trackingTargetWithoutCleanup == inconsistentTrackingInfo(numFV)) ||
        (if (!allSrcsEstablished) {
          false
        } else {
          val allVars = lab.getVars
          logger.debug("Checking establishment of " + allVars.mkString(", "))
          !allVars.exists(!EstablishmentAutomaton.isEstablished(trackingTargetWithoutCleanup, _))
        })

    // Drop the bound variables to get the resulting state of the tracking automaton
    val trackingTarget = dropNonFreeVariables(trackingTargetWithoutCleanup)
    logger.debug("Reached state (" + trackingTarget + "," + establishmentBit + ")")

    Set((trackingTarget, establishmentBit))
  }

}

object EstablishmentAutomaton {

  import BaseTrackingAutomaton.TrackingInfo

  /**
    * Is variable v established according to tracking info s?
    */
  def isEstablished(s : TrackingInfo, v : String) = {
    isFV(v) || s._1.contains(v) || s._2.exists({
      // Return true iff the pure atom witnesses that v is equal to a free variable
      // This is enough to show establishment, because we assume that s is congruence closed
      case PtrEq(l, r) => (l.toString == v && isFV(r)) || (r.toString == v && isFV(l))
      case _ => false
    })
  }

}
