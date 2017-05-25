package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.{PointsTo, PureAtom}

/**
  * Created by jkatelaa on 5/25/17.
  */
trait PartitionFilter {

  def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) : Boolean

}

trait RemoveEmpPartition extends PartitionFilter {

  override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = spatial.nonEmpty || pure.nonEmpty

}

trait KeepAllPartitions extends PartitionFilter {

  override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = true

}

trait KeepOnlyPartititionsWithPointers extends PartitionFilter {

  override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = spatial.nonEmpty

}