package slex.heapautomata

import slex.seplog.{PtrEq, SymbolicHeap}

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomaton(numFV : Int) extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import BaseTrackingAutomaton._

  override val description = "EST_" + numFV

  override type State = (TrackingInfo, Boolean)

  override val states: Set[State] = computeTrackingStateSpace(numFV) flatMap (s => Set((s,false), (s,true)))

  override def isFinal(s: State): Boolean = s._2

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {

    val trackingInfo = src map (_._1)
    // All src states are established iff there is no src state of the form (_,false)
    val allSrcsEstablished = !(src exists (!_._2))

    // Get compression + propagation including bound variables
    val trackingTargetWithoutCleanup = compressAndPropagate(trackingInfo, lab, inconsistentTrackingInfo(numFV))

    // Unless we already know that one of the children is not established,
    // check whether everything in that heap is either allocated or equal to a free variable
    val establishmentBit = if (!allSrcsEstablished) {
      false
    } else {
      val allVars = lab.getVars
      !allVars.exists(!EstablishmentAutomaton.isEstablished(trackingTargetWithoutCleanup, _))
    }

    // Drop the bound variables to get the resulting state of the tracking automaton
    val trackingTarget = dropNonFreeVariables(trackingTargetWithoutCleanup)

    Set((trackingTarget, establishmentBit))
  }

}

object EstablishmentAutomaton {

  import BaseTrackingAutomaton.TrackingInfo

  /**
    * Is variable v established according to tracking info s?
    */
  def isEstablished(s : TrackingInfo, v : String) = {
    s._1.contains(v) || s._2.exists({
      // Return true iff the pure atom witnesses that v is equal to a free variable
      // This is enough to show establishment, because we assume that s is congruence closed
      case PtrEq(l, r) => (l == v && isFV(r)) || (r == v && isFV(l))
      case _ => false
    })
  }

}
