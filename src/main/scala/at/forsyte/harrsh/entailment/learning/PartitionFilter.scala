package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.seplog.inductive.{PointsTo, PureAtom}

/**
  * Created by jkatelaa on 5/25/17.
  */
trait PartitionFilter extends LearningComponent {

  def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) : Boolean

}

object PartitionFilter {

  trait KeepAllPartitions extends PartitionFilter {

    override def componentDescriptions : Seq[String] = "keep all partitions" +: super.componentDescriptions

    override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = true

  }

  trait RemoveEmpPartition extends PartitionFilter {

    override def componentDescriptions : Seq[String] = "remove emp only" +: super.componentDescriptions

    override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = spatial.nonEmpty || pure.nonEmpty

  }

  trait KeepOnlyPartititionsWithPointers extends PartitionFilter {

    override def componentDescriptions : Seq[String] = "keep partitions with pointers" +: super.componentDescriptions

    override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = spatial.nonEmpty

  }

}