package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.TaggedAutomaton
import at.forsyte.harrsh.heapautomata.utils.{StateTag, TrackingInfo}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.{PtrEq, SymbolicHeap}
import com.typesafe.scalalogging.LazyLogging

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomaton(numFV : Int, acceptEstablished : Boolean) extends TaggedAutomaton[Boolean, TrackingInfo, BaseTrackingAutomaton] {

  override val baseAutomaton = BaseTrackingAutomaton.defaultTrackingAutomaton(numFV)

  override val tags = StateTag.instances.booleanTag

  override val description = (if (acceptEstablished) "EST_" else "NONEST_") + numFV

  override def isFinal(s: State) = tags.isFinalTag(s._2) == acceptEstablished

  override def tagComputation(srcTags : Seq[Boolean], lab : SymbolicHeap, baseTrg : baseAutomaton.State, trackingTargetWithoutCleanup : TrackingInfo) : Boolean = {
    if (!trackingTargetWithoutCleanup.isConsistent) {
      // Inconsistent heaps are regarded as established
      true
    } else {
      val allSrcsEstablished = !(srcTags exists (!_))

      // Unless we already know that one of the children is not established,
      // check whether everything in that heap is either allocated or equal to a free variable
      val establishmentBit =  if (!allSrcsEstablished) {
        false
      } else {
        val allVars = lab.allVars
        logger.debug("Checking establishment of " + allVars.mkString(", "))
        !allVars.exists(!isEstablished(trackingTargetWithoutCleanup, _))
      }

      logger.debug("Computed establishment bit: " + establishmentBit)

      establishmentBit
    }
  }

  /**
    * Is variable v established according to tracking info s?
    */
  private def isEstablished(s : TrackingInfo, v : Var) = {
    isFV(v) || s.alloc.contains(v) || s.pure.exists({
      // Return true iff the pure atom witnesses that v is equal to a free variable
      // This is enough to show establishment, because we assume that s is congruence closed
      case PtrEq(l, r) => (l.getVarOrZero == v && isFV(r.getVarOrZero)) || (r.getVarOrZero == v && isFV(l.getVarOrZero))
      case _ => false
    })
  }

}
