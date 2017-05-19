package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog._
import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 5/3/17.
  */
class EntailmentLearningLog(val reportProgress : Boolean) {

  private val SpacingBetweenProgressMessages : Int = 10

  private var learningEventLog : Seq[LearningEvent] = Seq()

  private object counters {
    var partitions : Int = 0
    var redents : Int = 0
    var tableLookups : Int = 0
    var tableUpdates : Int = 0

    def update(le : LearningEvent) : Unit = {
      le match {
        case ProcessPartition(partition) =>
          partitions = partitions + 1
        case TableLookupOperation(op) =>
          tableLookups = tableLookups + 1
        case TableUpdateOperation(op) =>
          tableUpdates = tableUpdates + 1
        case RedEntCheck(lhs, rhs, purpose) =>
          redents = redents + 1
        case _ =>
          // Nothing to count
      }
    }
  }
  
  private def eventBasedProgressReport(le : LearningEvent) : Unit = {
    if (le.isInstanceOf[ProcessPartition] && counters.partitions % SpacingBetweenProgressMessages == 0) {
      printProgress("Processed partitions: " + counters.partitions + " / Table updates: " + counters.tableUpdates + " / Successful table lookups: " + counters.tableLookups + " / Red. entailment checks: " + counters.redents)
    }
    le match {
      case operation: TableUpdateOperation =>
        // Table modification is reported immediately
        printProgress(le)
      case _ =>
        // Other events are not reported
    }
  }

  def printProgress(a : Any): Unit = if (reportProgress) println(a)

  def logEvent(le : LearningEvent): Unit = {
    counters.update(le)
    eventBasedProgressReport(le)
    learningEventLog = le +: learningEventLog
  }

  override def toString: String =
    learningEventLog.reverse.mkString("EntailmentLearning(\n", "\n", "\n)")

}

object EntailmentLearningLog {

  sealed trait LearningEvent {

    override def toString = this match {
      case IterationStats(iteration, numUnfs) =>
        "Iteration " + iteration + ": " + numUnfs + " unfoldings to process"
      case ProcessPartition(partition) =>
        "  Processing " + partition
      case TableLookupOperation(op) =>
        "    " + op
      case TableUpdateOperation(op) =>
        "    *** " + op
      case RedEntCheck(lhs, rhs, purpose) =>
        "      Checking " + lhs + " |?= " + rhs + " to decide " + purpose
      case ReachedFixedPoint() =>
        "Finished computation"
    }

  }

  case class IterationStats(iteration : Int, numUnfs : Int) extends LearningEvent
  case class ProcessPartition(partition : SymbolicHeapPartition) extends LearningEvent
  case class TableLookupOperation(op : TableOperations.LookupType) extends LearningEvent
  case class TableUpdateOperation(op : TableOperations.UpdateType) extends LearningEvent
  case class RedEntCheck(lhs : SymbolicHeap, rhs : SymbolicHeap, purpose : RedEntCheck.CheckPurpose) extends LearningEvent
  case class ReachedFixedPoint() extends LearningEvent

  object RedEntCheck {

    sealed trait CheckPurpose
    case class FinalityCheck() extends CheckPurpose {
      override def toString: String = "if underlying class is accepting"
    }
    case class ReducibilityCheck(reps : Set[SymbolicHeap]) extends CheckPurpose {
      override def toString: String = "reducibility to " + reps.mkString("{", ", ", "}")
    }
    case class ExtensionCompatibilityCheck(rep : SymbolicHeap, ext : SymbolicHeap) extends CheckPurpose {
      override def toString: String = "compatibility of " + rep + " with " + ext
    }

  }

  object TableOperations {

    sealed trait LookupType
    case class FoundEquivalent(sh : SymbolicHeap, equiv : TableEntry) extends LookupType {
      override def toString: String = "Found table entry " + equiv + " equivalent to " + sh
    }
    case class FoundReduction(sh : SymbolicHeap, entry : TableEntry) extends LookupType {
      override def toString: String = "Found reduction of " + sh + " to table entry " + entry
    }

    sealed trait UpdateType
    case class EnlargedEntry(entry : TableEntry, addition : SymbolicHeap, isNewRepresentative : Boolean) extends UpdateType {
      override def toString: String = "Extended " + entry + " with new " + (if (isNewRepresentative) "representative " else "extension ") + addition
    }
    case class NewEntry(entryId : Int, cleanedPartition: SymbolicHeapPartition) extends UpdateType {
      override def toString: String = "New table entry #" + entryId + ": " + cleanedPartition
    }
    case class SplitTableEntry(compatibleReps : Set[SymbolicHeap], incompatibleReps : Set[SymbolicHeap], triggeringExtension : SymbolicHeap) extends UpdateType {
      override def toString: String = "Splitting entry triggered by " + triggeringExtension + " yielding " + compatibleReps.mkString("{",", ","}") + " and " + incompatibleReps.mkString("{",", ","}")
    }
    case class MergeTableEntries(entries : Iterable[TableEntry]) extends UpdateType {
      override def toString: String = "Merging entries " + entries.mkString(", ")
    }

  }

}
