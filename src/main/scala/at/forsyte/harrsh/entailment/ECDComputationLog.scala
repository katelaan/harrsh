package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 3/30/17.
  */
trait ECDComputationLog {

  def incIteration : ECDComputationLog

  def logCurrentUnfolding(unf : SymbolicHeap) : ECDComputationLog

  def logEntailmentCheck(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]) : ECDComputationLog

  def logNewECD(ecdNr : Int, ecd : ECD) : ECDComputationLog

}

object ECDComputationLog {

  val completeLog : ECDComputationLog = CompleteECDComputationLog(Seq.empty, 0, None, Seq.empty)
  val dummyLog : ECDComputationLog = DummyECDComputationLog()

  private case class RedEntLogEntry(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]) {
    def indentedString(indent : String) : String = (
      indent + "checked against " + other.shortString + "\n"
        + indent + "  " + fstExt + " --> " + fstRes
        + sndExt.flatMap(ext => sndRes.map("\n" + indent + "  " + ext + " --> " + _ + "\n")).getOrElse("")
      )
  }

  private case class CompleteECDLogEntry(ecdNr : Int, ecd : ECD, iteration : Int, underlyingUnfolding : SymbolicHeap, checks : Seq[RedEntLogEntry]){
    override def toString = {
      val indent = "    "
      ("#" + ecdNr + ": " + ecd.shortString + "\n"
        + indent + "from " + iteration + "-unfolding " + underlyingUnfolding + " after " + checks.size + " checks" + (if (checks.nonEmpty) "\n" else "")
        + checks.map(_.indentedString(indent+indent)).mkString("\n"))
    }
  }

  private case class CompleteECDComputationLog(entries : Seq[CompleteECDLogEntry], currentIteration : Int, currentUnfolding : Option[SymbolicHeap], newChecks : Seq[RedEntLogEntry]) extends ECDComputationLog{

    override def incIteration = copy(currentIteration = currentIteration+1)

    override def logCurrentUnfolding(unf : SymbolicHeap) = copy(currentUnfolding = Some(unf))

    override def logEntailmentCheck(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]) = copy(newChecks = RedEntLogEntry(other,fstExt,fstRes=true,sndExt,sndRes) +: newChecks)

    override def logNewECD(ecdNr : Int, ecd : ECD) = copy(
      entries = CompleteECDLogEntry(ecdNr, ecd, currentIteration, currentUnfolding.get, newChecks.reverse) +: entries,
      newChecks = Seq.empty
    )

    override def toString = entries.reverse.mkString("\n\n")
  }

  private case class DummyECDComputationLog() extends ECDComputationLog {
    override def incIteration: ECDComputationLog = this
    override def logCurrentUnfolding(unf: SymbolicHeap): ECDComputationLog = this
    override def logEntailmentCheck(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]): ECDComputationLog = this
    override def logNewECD(ecdNr: Loc, ecd: ECD): ECDComputationLog = this

    override def toString = "Log deactivated"
  }

}
