package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.ExampleSIDs
import at.forsyte.harrsh.entailment.ForestsToLatex._
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive.{Predicate, RuleBody, SID}
import at.forsyte.harrsh.util.ToLatex._

import scala.language.implicitConversions

object ManualTreeConstruction {

  case class NodeDesc(succs: Seq[String], pred: Predicate, rule : Option[RuleBody], labels: Substitution)

  def makeTree(sid: SID, nodes: Map[String,NodeDesc], root: String, usedIds: Set[NodeId] = Set.empty): UnfoldingTree = {
    val strToNodeId : Map[String,NodeId] = nodes.keys.zipWithIndex.toMap
    val nodeLabels: Map[NodeId, NodeLabel] = nodes map {
      case (s, desc) => (strToNodeId(s), makeNodeLabel(desc))
    }
    val rootId = strToNodeId(root)
    val children = nodes map {
      case (s, desc) => (strToNodeId(s), desc.succs map strToNodeId)
    }
    UnfoldingTree(sid, nodeLabels, rootId, children)
  }

  def makeNodeLabel(nd: NodeDesc) : NodeLabel = nd.rule match {
    case None => AbstractLeafNodeLabel(nd.pred, nd.labels)
    case Some(rule) => RuleNodeLabel(nd.pred, rule, nd.labels)
  }

  implicit def makeSubst(tuple: (Predicate,Seq[String])): Substitution = {
    val (pred, subst) = tuple
    val substMap = (pred.params, subst.map(s => Set(FreeVar(s)))).zipped.toMap
    Substitution(substMap)
  }

  implicit def strToVar(s: String): FreeVar = FreeVar(s)

  def main(args: Array[String]) : Unit = {
    val sid = ExampleSIDs.Tll
    val pred = sid(sid.startPred)
    val rules = pred.rules
    val baseRule = rules.find(_.isBaseRule).get
    val recRule = rules.find(_.isRecRule).get

    def mk(desc: (String,NodeDesc)*) = makeTree(sid, desc.toMap, "root")

    val t1 = mk(
      ("root", NodeDesc(Seq("l", "r"), pred, Some(recRule), (pred, Seq("x", "?1", "?2")))),
      ("l", NodeDesc(Seq.empty, pred, None, (pred, Seq("y", "?1", "?3")))),
      ("r", NodeDesc(Seq.empty, pred, None, (pred, Seq("z", "?3", "?2")))))

    val t21 = mk(
      ("root", NodeDesc(Seq("l", "r"), pred, Some(recRule), (pred, Seq("y", "a", "?1")))),
      ("l", NodeDesc(Seq.empty, pred, Some(baseRule), (pred, Seq("a", "a", "?2")))),
      ("r", NodeDesc(Seq.empty, pred, Some(baseRule), (pred, Seq("?2", "?2", "?1")))))

    val t22 = mk(
      ("root", NodeDesc(Seq("l", "r"), pred, Some(recRule), (pred, Seq("z", "?1", "b")))),
      ("l", NodeDesc(Seq.empty, pred, Some(baseRule), (pred, Seq("?1", "?1", "?2")))),
      ("r", NodeDesc(Seq.empty, pred, Some(baseRule), (pred, Seq("?2", "?2", "b")))))

    for {
      ut <- Seq(t1, t21, t22)
    } {
      //println(ut)
      println(ut.toLatex)
      println()
    }

    val uf = UnfoldingForest(Set(t1, t21, t22))
    println(uf.toLatex)

    println("\n\nAfter composition:\n\n")
    val composed = t1.compose(t21).map(_._1).get
    //println(composed)
    println(composed.toLatex)

    println("\n\nAfter second composition:\n\n")
    val finished = composed.compose(t22).map(_._1).get
    //println(finished)
    println(finished.toLatex)
  }



}
