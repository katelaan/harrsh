package slex.seplog

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, rules : Set[(String, SymbolicHeap)], description : String = "Unnamed SID") {

  override def toString = {
    description + " (start predicate '" + startPred + "'): " + rules.toSeq.sortBy(_._1).map(p => p._1 + " <= " + p._2).mkString("\n    ", "\n    ", "")
  }

  /**
    * Is the language of this SID empty?
    */
  def isLanguageEmpty : Boolean = ??? // TODO: Implement SID emptiness

}

object SID {

  def apply(startPred : String, description : String, rules : (String, SymbolicHeap)*) = new SID(startPred, Set()++rules, description)

}