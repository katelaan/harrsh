package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.TaggedAutomaton
import at.forsyte.harrsh.heapautomata.utils.{StateTag, TrackingInfo}
import at.forsyte.harrsh.refinement.AutomatonTask
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PureAtom, SymbolicHeap}

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomaton(acceptEstablished : Boolean) extends TaggedAutomaton[Boolean, TrackingInfo, BaseTrackingAutomaton] {

  override val baseAutomaton = BaseTrackingAutomaton.defaultTrackingAutomaton

  override val tags = StateTag.instances.booleanTag

  override val description = (if (acceptEstablished) AutomatonTask.keywords.est else AutomatonTask.keywords.nonest)

  override def isFinal(s: State) = tags.isFinalTag(s._2) == acceptEstablished

  override def tagComputation(srcTags : Seq[Boolean], lab : SymbolicHeap, baseTrg : baseAutomaton.State, trackingTargetWithoutCleanup : TrackingInfo) : Boolean = {
    if (!trackingTargetWithoutCleanup.isConsistent) {
      // Inconsistent heaps are regarded as established
      logger.debug(s"Regarding inconsistent target $trackingTargetWithoutCleanup as established")
      true
    } else {
      val allSrcsEstablished = !(srcTags exists (!_))

      // Unless we already know that one of the children is not established,
      // check whether everything in that heap is either allocated or equal to a free variable
      val establishmentBit =  if (!allSrcsEstablished) {
        logger.debug(s"There is a non-established source among $srcTags")
        false
      } else {
        val allVars = lab.allNonNullVars
        logger.debug("Checking establishment of " + allVars.mkString(", "))
        allVars.forall(isEstablished(trackingTargetWithoutCleanup, _))
      }

      logger.debug(s"Computed establishment bit: $establishmentBit")

      establishmentBit
    }
  }

  /**
    * Is variable v established according to tracking info s?
    */
  private def isEstablished(s : TrackingInfo, v : Var) = {
    v.isFree || s.alloc.contains(v) || s.pure.exists({
      // Return true iff the pure atom witnesses that v is equal to a free variable
      // This is enough to show establishment, because we assume that s is congruence closed
      case PureAtom(l, r, isEquality) => isEquality && ((l == v && r.isFree) || (r == v && l.isFree))
    })
  }

}
