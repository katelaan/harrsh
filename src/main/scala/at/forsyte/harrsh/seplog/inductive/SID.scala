package at.forsyte.harrsh.seplog.inductive

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, rules : Set[(String, SymbolicHeap)], description : String = "Unnamed SID") {

  override def toString = {
    description + " (start predicate '" + startPred + "'): " + rules.toSeq.sortBy(_._1).map(p => p._1 + " <= " + p._2).mkString("\n    ", "\n    ", "")
  }

  // TODO Should we record the arity of the predicates explicitly?
  def arityOfStartPred : Int = rules.filter(_._1 == startPred).map(rule => rule._2.getVars.size - rule._2.qvars.size).max

}

object SID {

  def apply(startPred : String, description : String, rules : (String, SymbolicHeap)*) = new SID(startPred, Set()++rules, description)

}