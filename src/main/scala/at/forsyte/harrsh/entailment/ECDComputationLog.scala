package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 3/30/17.
  */
trait ECDComputationLog {

  var unfCounter : Int
  var partCounter : Int
  var redEntCounter : Int

  def setEntailmentRightHandSide(callToStartPred: SymbolicHeap): ECDComputationLog

  def incIteration : ECDComputationLog

  def logCurrentUnfolding(unf : SymbolicHeap) : ECDComputationLog

  def logEntailmentCheck(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]) : ECDComputationLog

  def logNewECD(ecdNr : Int, ecd : ECD) : ECDComputationLog

  def incUnfCounter : Unit = unfCounter = unfCounter + 1
  def incPartCounter : Unit = partCounter = partCounter + 1
  def incRedEntCounter : Unit = {
    redEntCounter = redEntCounter + 1
    if (redEntCounter % 100 == 0) println(statistics)
  }

  def statistics : String = "Processed " + unfCounter + " unfoldings with " + partCounter + " partitions, running a total of " + redEntCounter + " reduced entailment checks"

}

object ECDComputationLog {

  val completeLog : ECDComputationLog = CompleteECDComputationLog(SymbolicHeap.empty, Seq.empty, 0, None, Seq.empty)
  val dummyLog : ECDComputationLog = DummyECDComputationLog()

  private case class RedEntLogEntry(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]) {
    def indentedString(indent : String, rhs : SymbolicHeap) : String = (
      indent + "checked against " + other.shortString + "\n"
        + indent + "  " + fstExt + " |= " + rhs + " --> " + fstRes +
      (for {
        sndExtV <- sndExt
        sndResV <- sndRes
      } yield  "\n" + indent + "  " + sndExtV + " --> " + sndResV).getOrElse(""))
  }

  private case class CompleteECDLogEntry(ecdNr : Int, ecd : ECD, iteration : Int, underlyingUnfolding : SymbolicHeap, checks : Seq[RedEntLogEntry]){
    def indentedString(indent : String, rhs : SymbolicHeap) = {
      ("#" + ecdNr + ": " + ecd.shortString + "\n"
        + indent + "from " + iteration + "-unfolding " + underlyingUnfolding + " after " + checks.size + " checks" + (if (checks.nonEmpty) "\n" else "")
        + checks.map(_.indentedString(indent+indent, rhs)).mkString("\n"))
    }
  }

  private case class CompleteECDComputationLog(entailmentRhs : SymbolicHeap, entries : Seq[CompleteECDLogEntry], currentIteration : Int, currentUnfolding : Option[SymbolicHeap], newChecks : Seq[RedEntLogEntry], override var unfCounter : Int = 0, override var partCounter : Int = 0, override var redEntCounter : Int = 0) extends ECDComputationLog{

    override def incIteration = copy(currentIteration = currentIteration+1)

    override def setEntailmentRightHandSide(entailmentRhs: SymbolicHeap): ECDComputationLog = copy(entailmentRhs = entailmentRhs)

    override def logCurrentUnfolding(unf : SymbolicHeap) = copy(currentUnfolding = Some(unf))

    override def logEntailmentCheck(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]) = copy(newChecks = RedEntLogEntry(other,fstExt,fstRes,sndExt,sndRes) +: newChecks)

    override def logNewECD(ecdNr : Int, ecd : ECD) = copy(
      entries = CompleteECDLogEntry(ecdNr, ecd, currentIteration, currentUnfolding.get, newChecks.reverse) +: entries,
      newChecks = Seq.empty
    )

    override def toString = entries.reverse.map(_.indentedString("    ", entailmentRhs)).mkString("\n\n")
  }

  private case class DummyECDComputationLog(override var unfCounter : Int = 0, override var partCounter : Int = 0, override var redEntCounter : Int = 0) extends ECDComputationLog {
    override def incIteration: ECDComputationLog = this
    override def setEntailmentRightHandSide(entailmentRhs: SymbolicHeap): ECDComputationLog = this
    override def logCurrentUnfolding(unf: SymbolicHeap): ECDComputationLog = this
    override def logEntailmentCheck(other : ECD, fstExt: SymbolicHeap, fstRes : Boolean, sndExt: Option[SymbolicHeap], sndRes : Option[Boolean]): ECDComputationLog = this
    override def logNewECD(ecdNr: Loc, ecd: ECD): ECDComputationLog = this

    override def toString = "[Detailed log deactivated, activate by setting EnableDetailedLog = true]"
  }

}
