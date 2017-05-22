package at.forsyte.harrsh.util.export

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PointsTo, PredCall, PtrNEq, SymbolicHeap}

/**
  * Created by jens on 5/21/17.
  */
object SymbolicHeapToDotGraph extends HarrshLogging {

  def apply(sh : SymbolicHeap) : String = {
    val closure = Closure.ofSetOfAtoms(sh.pure.toSet)
    val hasNull = sh.usedFreeVars.contains(Var.nil)
    val relevantVars = sh.allVars ++ (if (hasNull) Some(Var.nil) else None)
    val representatives = closure.classRepresentativesOf(relevantVars)

    "digraph G{\n" +
      GlobalStyle + "\n" +
      (varsToNodes(representatives, closure).toSeq ++
        callsToNodes(sh.predCalls) ++
        pointersToEdges(sh.pointers, closure) ++
        inequalitiesToEdges(sh.pure.filter(_.isInstanceOf[PtrNEq]).map(_.asInstanceOf[PtrNEq]), closure)).mkString("", "\n", "\n") +
      "}"
  }

  private def varsToNodes(vars : Iterable[Var], cl : Closure) : Iterable[String] = vars map {
    v => v + " " + mkAttr(varStyle(v, cl))
  }

    private def callsToNodes(calls : Iterable[PredCall]) : Iterable[String] = calls map {
    c => (c.name + Math.abs(c.hashCode)) + " " + mkAttr(callStyle(c))
  }

  private def varStyle(v: Var, cl : Closure) : Iterable[String] = {
    val classOfV = cl.getEquivalenceClass(v)
    val style = if (classOfV.contains(Var.nil)) {
      Seq("shape=none")
    } else if (classOfV.exists(_.isFree)) {
      Seq("shape=box", "fillcolor=\"#6666dd\"")
    } else {
      Seq("shape=circle","fillcolor=\"#66dd66\"")
    }

    ("label=\""+cl.getEquivalenceClass(v).mkString(", ")+"\"") +: style
  }

  private def callStyle(c: PredCall) : Iterable[String] = {
    Seq("label=\"" + c + "\"", "fillcolor=\"#dd6666\"")
  }

  private def pointersToEdges(ptrs : Iterable[PointsTo], cl : Closure) : Iterable[String] = ptrs flatMap {
    p => p.toAsVarOrZero.map(cl.getRepresentative).zipWithIndex.map{
      case (toRep,ix) => p.fromAsVar + " -> " + toRep + " " + mkAttr(Seq("label=" + (ix+1)))
    }
      //cl.getRepresentative(p.fromAsVar) + " -> " + .mkString("{", " ", "}")
  }

  private def inequalitiesToEdges(ineqs : Iterable[PtrNEq], cl : Closure) : Iterable[String] = ineqs map {
    case neq@PtrNEq(l, r) =>
      val label = "label=\"" + neq + "\""
      cl.getRepresentative(l.getVarOrZero) + " -> " + cl.getRepresentative(r.getVarOrZero) + " " + mkAttr(label +: InequalityStyle)

  }

  private def mkAttr(as : Iterable[String]) : String = as.mkString("[",", ","]")

  private val InequalityStyle : Seq[String] = Seq("style=dotted","dir=none")

  // [bgcolor="transparent"]
  private def GlobalStyle : String = """graph [bgcolor="#DDDDDD"]
 node [fontname=Verdana,fontsize=12]
 node [style=filled]
 node [fillcolor="#FFFFFF"]
 node [color="#EEEEEE"]"""

}
