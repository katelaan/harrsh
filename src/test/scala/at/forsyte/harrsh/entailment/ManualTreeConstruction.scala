package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.ExampleSIDs
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RuleBody, SID}
import at.forsyte.harrsh.util.ToLatex._
import at.forsyte.harrsh.entailment.CanCompose._

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
    UnfoldingTree(nodeLabels, rootId, children, convertToNormalform = false)
  }

  def makeNodeLabel(nd: NodeDesc) : NodeLabel = nd.rule match {
    case None => AbstractLeafNodeLabel(nd.pred, nd.labels)
    case Some(rule) => RuleNodeLabel(nd.pred, rule, nd.labels)
  }

  implicit def makeSubst(vars: Seq[String]): Substitution = {
    Substitution(vars map (s => Set[Var](FreeVar(s))))
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
      ("root", NodeDesc(Seq("l", "r"), pred, Some(recRule), Seq("x", "?1", "?2"))),
      ("l", NodeDesc(Seq.empty, pred, None, Seq("y", "?1", "?3"))),
      ("r", NodeDesc(Seq.empty, pred, None, Seq("z", "?3", "?2"))))

    val t21 = mk(
      ("root", NodeDesc(Seq("l", "r"), pred, Some(recRule), Seq("y", "a", "?1"))),
      ("l", NodeDesc(Seq.empty, pred, Some(baseRule), Seq("a", "a", "?2"))),
      ("r", NodeDesc(Seq.empty, pred, Some(baseRule), Seq("?2", "?2", "?1"))))

    val t22 = mk(
      ("root", NodeDesc(Seq("l", "r"), pred, Some(recRule), Seq("z", "?1", "b"))),
      ("l", NodeDesc(Seq.empty, pred, Some(baseRule), Seq("?1", "?1", "?2"))),
      ("r", NodeDesc(Seq.empty, pred, Some(baseRule), Seq("?2", "?2", "b"))))

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
    val composed = (t1 compose t21).get
    //println(composed)
    println(composed.toLatex)

    println("\n\nAfter second composition:\n\n")
    val finished = (composed compose t22).get
    //println(finished)
    println(finished.toLatex)

    println("Manual result of second composition:\n")
    println(finished)
    println("Direct forest composition:\n\n")
    val direct = UnfoldingForest(Set(t1)).compose(UnfoldingForest(Set(t21,t22)))
    println(direct)

    println(s"Results are the same: ${finished == direct.trees.head}")

    println(UnfoldingForest(Set(t1)).toExtensionTypeWithoutDisequalities)
    println(direct.toExtensionTypeWithoutDisequalities)
  }



}
