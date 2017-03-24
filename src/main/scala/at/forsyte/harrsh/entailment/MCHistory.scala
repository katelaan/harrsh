package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{NullPtr, PtrVar, _}
import at.forsyte.harrsh.seplog.inductive.{PointsTo, PtrEq, PtrNEq, SymbolicHeap}

// TODO Track additional equalities imposed by matching free vars in pointer matching? (Already in the History data structure, but not set anywhere.)
case class MCHistory(iteration: Int, alloc : Set[Var], pure : Set[PtrEq], stepLogger : MCHistory.StepLogger) extends HarrshLogging {
  def addAlloc(v : Var) = copy(alloc = alloc + v)
  def addPure(p : PtrEq) = copy(pure = pure + p)
  def nextIteration = copy(iteration = iteration+1)

  def toFormula = {
    logger.debug("Generating formula from allocation set " + alloc + " and additional pure constraints " + pure)
    val contradiction = pure.find(eq => alloc.contains(eq.l.getVarOrZero) && alloc.contains(eq.r.getVarOrZero))
    val contradictoryConstraint = contradiction match {
      case Some(eq@PtrEq(l,r)) =>
        logger.debug("CONTRADICTORY CONSTRAINT: " + eq + ", but both allocated")
        Seq(PtrNEq(l,r))
      case None => Seq.empty
    }

    val allocPure = alloc map (v => PtrNEq(PtrVar(v), NullPtr()))
    allocPure ++ contradictoryConstraint
  }

  def logStep(step : MCHistory.ModelCheckingStep) = copy(stepLogger = stepLogger.logStep(step))
}

object MCHistory {

   val emptyHistory = MCHistory(0, Set.empty, Set.empty, DefaultStepLogger())

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
