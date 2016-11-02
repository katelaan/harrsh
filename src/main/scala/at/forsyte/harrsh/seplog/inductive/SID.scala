package at.forsyte.harrsh.seplog.inductive

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, rules : Set[Rule], description : String = "Unnamed SID") {

  override def toString = {
    description + " (start predicate '" + startPred + "'): " + rules.toSeq.sortBy(_.head).mkString("\n    ", "\n    ", "")
  }

  // TODO Should we record the arity of the predicates explicitly?
  def arityOfStartPred : Int = rules.filter(_.head == startPred).map(rule => rule.freeVars.size).max

}

object SID {

  def apply(startPred : String, description : String, rules : (String, Seq[String], SymbolicHeap)*) = new SID(startPred, Set()++(rules map Rule.fromTuple), description)

}