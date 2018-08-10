package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.ExampleSIDs
import at.forsyte.harrsh.entailment.ForestsToLatex._
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, Rule, SID}
import at.forsyte.harrsh.util.ToLatex._

import scala.language.implicitConversions

object ManualTreeConstruction {

  case class NodeDesc(succs: Seq[String], content: Either[Predicate, Rule], labels: Labeling)

  def makeTree(sid: SID, nodes: Map[String,NodeDesc], root: String): UnfoldingTree = {
    val currNode = nodes(root)
    val subTrees = currNode.succs map (makeTree(sid, nodes, _))
    val rootNode = makeNode(currNode)
    val rootChildren = currNode.succs.map(nodes).map(makeNode)
    val rootChildMap = Map(rootNode -> rootChildren)
    val allNodes = Set(rootNode) ++ subTrees.flatMap(_.nodes)
    val fullChildMap = rootChildMap ++ subTrees.flatMap(_.children)
    UnfoldingTree(sid, allNodes, rootNode, fullChildMap)
  }

  def makeNode(nd: NodeDesc) : UTNode = nd match {
    case NodeDesc(_, Left(pred), labels) => AbstractLeafNode(pred, labels)
    case NodeDesc(_, Right(rule), labels) => RuleNode(rule, labels)
  }

  implicit def mkLabeling(tuple: (Predicate,Seq[String])): Labeling = {
    val (pred, subst) = tuple
    val labelingMap = (pred.params, subst.map(s => Set[Var](FreeVar(s)))).zipped.toMap
    Labeling(labelingMap)
  }

  implicit def strToVar(s: String): FreeVar = FreeVar(s)

  def main(args: Array[String]) : Unit = {
    val sid = ExampleSIDs.Tll
    val pred = sid.preds(sid.startPred)
    val rules = sid.rules
    val baseRule = rules.find(_.isBaseRule).get
    val recRule = rules.find(_.isRecRule).get

    def mk(desc: (String,NodeDesc)*) = makeTree(sid, desc.toMap, "root")

    val t1 = mk(
      ("root", NodeDesc(Seq("l", "r"), Right(recRule), (pred, Seq("x", "?", "?")))),
      ("l", NodeDesc(Seq.empty, Left(pred), (pred, Seq("y", "?", "?")))),
      ("r", NodeDesc(Seq.empty, Left(pred), (pred, Seq("z", "?", "?")))))

    val t21 = mk(
      ("root", NodeDesc(Seq("l", "r"), Right(recRule), (pred, Seq("y", "a", "?")))),
      ("l", NodeDesc(Seq.empty, Right(baseRule), (pred, Seq("a", "a", "?")))),
      ("r", NodeDesc(Seq.empty, Right(baseRule), (pred, Seq("?", "?", "?")))))

    val t22 = mk(
      ("root", NodeDesc(Seq("l", "r"), Right(recRule), (pred, Seq("z", "?", "b")))),
      ("l", NodeDesc(Seq.empty, Right(baseRule), (pred, Seq("?", "?", "?")))),
      ("r", NodeDesc(Seq.empty, Right(baseRule), (pred, Seq("?", "?", "b")))))

    for {
      ut <- Seq(t1, t21, t22)
    } {
      println(ut.toLatex)
      println()
    }

    val uf = UnfoldingForest(Set(t1, t21, t22))
    println(uf.toLatex)

    println("\n\nAfter composition:\n\n")

    println(t1.compose(t21).map(_._1).get.toLatex)
  }



}
