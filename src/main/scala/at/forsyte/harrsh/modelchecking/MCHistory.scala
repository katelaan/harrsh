package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConsistencyCheck
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._

/**
  * Logs the history of the model checking computation, including which allocated variables have been dropped in matching
  * @param iteration Current iteration of the model checker
  * @param alloc Vars that have been allocated on the left-hand side, but have been removed in pointer matching; saved as sequence rather than set to guarantee that double allocation will lead to inconsistency
  * @param stepLogger Logger for debugging
  */
case class MCHistory(iteration: Int, alloc : Seq[Var], stepLogger : MCHistory.StepLogger) extends HarrshLogging {

  def addAlloc(v : Var): MCHistory = {
    logger.debug("Adding allocation info to history: " + v)
    copy(alloc = alloc :+ v)
  }

  def nextIteration: MCHistory = copy(iteration = iteration+1)

  def toPureConstraints : Iterable[PureAtom] = {
    logger.debug("Generating formula from allocation set " + alloc.mkString("{",",","}")) // + " and additional pure constraints " + pure)
    // TODO Is there any reason to log pure constraints as well? I don't think so, but let's keep the code for the time being
//    val contradiction = pure.find(eq => alloc.contains(eq.l.getVarOrZero) && alloc.contains(eq.r.getVarOrZero))
//    contradiction match {
//      case Some(eq@PtrEq(l,r)) =>
//        logger.debug("CONTRADICTORY CONSTRAINT: " + eq + ", but both allocated")
//        Seq(PtrNEq(l,r))
//      case None => ConsistencyCheck.allocationInfoToConsistencyConstraints(alloc)
//    }
    ConsistencyCheck.allocationInfoToConsistencyConstraints(alloc)
  }

  def logStep(step : MCHistory.ModelCheckingStep) = copy(stepLogger = stepLogger.logStep(step))
}

object MCHistory {

  val emptyHistory = MCHistory(0, Seq.empty, DefaultStepLogger())

  sealed trait ModelCheckingStep
  case class PointerMatch(lhs : SymbolicHeap, rhs : SymbolicHeap, lhsPtr : PointsTo, rhsPtr : PointsTo) extends ModelCheckingStep {
    override def toString = lhs + " |= " + rhs +  " ---[MATCHING]---> " + lhsPtr + " ~ " + rhsPtr
  }
  case class UnfoldingStep(lhs : SymbolicHeap, rhs : SymbolicHeap, unfoldedRhs : SymbolicHeap) extends ModelCheckingStep {
    override def toString = lhs + " |= " + rhs + " ---[UNFOLDING]---> " + unfoldedRhs
  }

  trait StepLogger {
    def logStep(step : ModelCheckingStep) : StepLogger
  }

  case class DummyStepLogger() extends StepLogger {
    override def logStep(step: ModelCheckingStep): StepLogger = this
    override def toString = "[Step log deactivated]"
  }
  case class DefaultStepLogger(reverseSteps : Seq[ModelCheckingStep]) extends StepLogger {
    override def logStep(step: ModelCheckingStep): StepLogger = DefaultStepLogger(step +: reverseSteps)
    override def toString = {
      val lines = for {
        (step,i) <- reverseSteps.reverse.zipWithIndex
      } yield "" + (i+1) + ": " + step
      lines.mkString("\n")
    }
  }
  object DefaultStepLogger {
    def apply() : DefaultStepLogger = DefaultStepLogger(Seq.empty)
  }

}
