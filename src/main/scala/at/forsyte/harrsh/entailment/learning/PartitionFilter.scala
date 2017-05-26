package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.seplog.inductive.{PointsTo, PureAtom}

/**
  * Created by jkatelaa on 5/25/17.
  */
trait PartitionFilter {

  def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) : Boolean

}
