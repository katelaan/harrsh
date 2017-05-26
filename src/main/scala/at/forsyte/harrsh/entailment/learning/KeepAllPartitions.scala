package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.seplog.inductive.{PointsTo, PureAtom}

/**
  * Created by jkatelaa on 5/26/17.
  */
trait KeepAllPartitions extends PartitionFilter {

  override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = true

}
