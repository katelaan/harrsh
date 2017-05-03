package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog._
import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 5/3/17.
  */
class EntailmentLearningLog(val reportProgress : Boolean) {

  var learningEventLog : Seq[LearningEvent] = Seq()

  def printProgress(a : Any): Unit = if (reportProgress) println(a)

  def logEvent(le : LearningEvent): Unit = learningEventLog = le +: learningEventLog

  override def toString: String =
    learningEventLog.reverse.map {
    case IterationStats(iteration, numUnfs) =>
      "Iteration " + iteration + ": " + numUnfs + " unfoldings to process"
    case ProcessPartition(partition) =>
      "  Processing " + partition
    case TableOperation(op) =>
      "    " + op
    case RedEntCheck(lhs, rhs, purpose) =>
      "      Checking " + lhs + " |?= " + rhs + " to decide " + purpose
    case MergeTableEntries(entries) =>
      "  Merging entries:\n" + entries.map("   - " + _).mkString("\n")
    case ReachedFixedPoint() =>
      "Finished computation"
  }.mkString("EntailmentLearning(\n", "\n", "\n)")

}

object EntailmentLearningLog {

  sealed trait LearningEvent

  case class IterationStats(iteration : Int, numUnfs : Int) extends LearningEvent
  case class ProcessPartition(partition : SymbolicHeapPartition) extends LearningEvent
  case class TableOperation(op : TableOperation.Type) extends LearningEvent
  case class RedEntCheck(lhs : SymbolicHeap, rhs : SymbolicHeap, purpose : RedEntCheck.CheckPurpose) extends LearningEvent
  case class MergeTableEntries(entries : Iterable[TableEntry]) extends LearningEvent
  case class ReachedFixedPoint() extends LearningEvent

  object RedEntCheck {

    sealed trait CheckPurpose
    case class FinalityCheck() extends CheckPurpose {
      override def toString: String = "final state"
    }
    case class ReducibilityCheck(reps : Set[SymbolicHeap]) extends CheckPurpose {
      override def toString: String = "reducibility to " + reps.mkString("{", ", ", "}")
    }

  }

  object TableOperation {

    sealed trait Type
    case class FoundEquivalent(equiv : TableEntry) extends Type {
      override def toString: String = "Found equivalent table entry " + equiv
    }
    case class FoundReduction(entry : TableEntry) extends Type {
      override def toString: String = "Found reduction to table entry " + entry
    }
    case class ExtendedEntry(entry : TableEntry, ext : SymbolicHeap) extends Type {
      override def toString: String = "*** Extended " + entry + " with " + ext
    }
    case class NewEntry(cleanedPartition: SymbolicHeapPartition) extends Type {
      override def toString: String = "*** New entry " + cleanedPartition
    }

  }

}
