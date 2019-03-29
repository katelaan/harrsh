package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.inductive.SidLike
import at.forsyte.harrsh.seplog.sidtransformers.GraphInterfaceAutomaton.GraphInterface

object GraphInterfaceAnnotator extends HarrshLogging {

  def apply(sid: SidLike): Map[String, GraphInterfaceAutomaton.GraphInterface] = {
    val fixedPoint = RefinementAlgorithms.fullRefinementTrace(sid, GraphInterfaceAutomaton)
    for {
      (pred, interfaces) <- fixedPoint._1
      intersection <- intersectAll(interfaces)
    } yield (pred, intersection)

  }

  private def intersectAll(interfaces: Set[GraphInterface]): Option[GraphInterface] = {
    val nonEmptyIfs = interfaces.filterNot(_.isEmptyGraph)
    logger.warn("Will intersect the following interfaces:\n" + nonEmptyIfs.mkString("\n"))
    if (nonEmptyIfs.nonEmpty) {
      Some(nonEmptyIfs.reduce(GraphInterface.intersect))
    } else {
      None
    }
  }

}
