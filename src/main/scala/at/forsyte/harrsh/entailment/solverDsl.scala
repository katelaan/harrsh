package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentChecker.{EntailmentCheckerResult, EntailmentCheckerStats}
import at.forsyte.harrsh.main.ProblemStatus
import at.forsyte.harrsh.main.ProblemStatus.{Correct, Incorrect, Unknown}

case class SolverStateTransformer[State1, State2](run: (EntailmentInstance, State1) => State2) {

  def andThen(ss: SolverStrategy[State2]): SolverStrategy[State1] = Transform(this, ss)

  def apply(ei: EntailmentInstance, state: State1): State2 = run(ei, state)

}

case class StatsGenerator[State](run: (EntailmentInstance, State) => EntailmentCheckerStats) {

  def andThen(ss: SolverStrategy[State]) = AddStats(this, ss)

  def apply(ei: EntailmentInstance, state: State): EntailmentCheckerStats = run(ei, state)

}

object SolverStateTransformer {

  def addToState[A,B](f: (EntailmentInstance, A) => B): SolverStateTransformer[A,(A,B)] = {
    SolverStateTransformer((ei, a) => (a,f(ei, a)))
  }

  def fromFunction[B](f: EntailmentInstance => B): SolverStateTransformer[Unit, B] = {
    SolverStateTransformer((ei,_) => f(ei))
  }

  def fromFunction[A,B](f: (EntailmentInstance, A) => B): SolverStateTransformer[A, B] = {
    SolverStateTransformer(f)
  }

}

sealed trait SolverStrategy[State] {

  def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult

  def orElse(ss: SolverStrategy[State]): SolverStrategy[State] = {
    OrElse(this, ss)
  }

}

object SolverStrategy {

  def chain[State](strat: SolverStrategy[State], otherStrats: SolverStrategy[State]*): SolverStrategy[State] = {
    (strat +: otherStrats).reduceLeft(_ orElse _)
  }

  def fromFunction[State](f : (EntailmentInstance, State) => ProblemStatus): SolverStrategy[State] = Tactic(f)

  def fromFunction(f : EntailmentInstance => ProblemStatus): SolverStrategy[Unit] = Tactic((ei, _) => f(ei))

}

case class Constantly[State](status: ProblemStatus) extends SolverStrategy[State] {
  override def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult = {
    EntailmentCheckerResult(status, None)
  }
}

case class Tactic[State](run: (EntailmentInstance, State) => ProblemStatus) extends SolverStrategy[State] {

  override def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult = {
    EntailmentCheckerResult(run(ei, state), None)
  }

}

case class GuardedStrategy[State](guard: Boolean, ss: SolverStrategy[State]) extends SolverStrategy[State] {

  override def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult = {
    if (guard) ss(ei, state) else EntailmentCheckerResult(Unknown, None)
  }

  override def orElse(ss: SolverStrategy[State]): SolverStrategy[State] = {
    if (!guard) ss else OrElse(this, ss)
  }

}

case class Conditional[State](condition: (EntailmentInstance, State) => Boolean, thenBranch: SolverStrategy[State], elseBranch: SolverStrategy[State]) extends SolverStrategy[State] {

  override def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult = {
    if (condition(ei, state)) thenBranch(ei, state) else elseBranch(ei, state)
  }

}


case class AddStats[State](statsGenerator: StatsGenerator[State], ss: SolverStrategy[State]) extends SolverStrategy[State] {
override def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult = {
    ss(ei, state).copy(maybeStats = Some(statsGenerator(ei, state)))
  }

}

case class Transform[State1, State2](transformer: SolverStateTransformer[State1, State2], ss: SolverStrategy[State2]) extends SolverStrategy[State1] {

  override def apply(ei: EntailmentInstance, state: State1): EntailmentCheckerResult = {
    ss(ei, transformer(ei, state))
  }

}

case class SplitAndThen[State](splitter: EntailmentInstance => Seq[EntailmentInstance], ss: SolverStrategy[State]) extends SolverStrategy[State] {

  override def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult = {
    val ress = splitter(ei) map (ss(_, state))
    val statuses = ress.map(_.status)
    assert(ress.map(_.maybeStats).toSet.size == 1, "All subinstances should produce the same stats, because we currently don't have support for integrating stats")
    val outStats = ress.head.maybeStats
    if (statuses.exists(_.isUnknown)) {
      EntailmentCheckerResult(Unknown, outStats)
    } else if (statuses.forall(_.isCorrect)) {
      EntailmentCheckerResult(Correct, outStats)
    } else {
      EntailmentCheckerResult(Incorrect, outStats)
    }
  }

}

case class OrElse[State](fst: SolverStrategy[State], snd: SolverStrategy[State]) extends SolverStrategy[State] {

  override def apply(ei: EntailmentInstance, state: State): EntailmentCheckerResult = {
    val res = fst(ei, state)
    if (res.status.isUnknown) {
      snd(ei, state)
    } else {
      res
    }
  }

}
