package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.heapautomata.utils.StateTag
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/29/17.
  */
abstract class TaggedAutomaton[A,E, B <: HeapAutomaton with InconsistentState with TaggedTargetComputation[E]] extends HeapAutomaton with InconsistentState {

  val baseAutomaton : B

  val tags : StateTag[A]

  def tagComputation(srcTags : Seq[A], lab : SymbolicHeap, baseTrg : baseAutomaton.State, extraInfo : E) : A

  override final type State = (baseAutomaton.State, A)

  override def inconsistentState(fvs: Seq[FreeVar]) = (baseAutomaton.inconsistentState(fvs), tags.inconsistentTag)

  override def isFinal(s: State) = tags.isFinalTag(s._2)

  override final def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    val baseSrc = src map (_._1)
    val baseTrgsWithExtraInfo = baseAutomaton.getTargetsWithTags(baseSrc, lab)

    for {
      (baseTrg,extraInfo) <- baseTrgsWithExtraInfo
      trgTag = tagComputation(src map (_._2), lab, baseTrg, extraInfo)
    } yield (baseTrg, trgTag)
  }

}
