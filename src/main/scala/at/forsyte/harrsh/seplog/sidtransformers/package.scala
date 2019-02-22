package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.inductive.Predicate

package object sidtransformers {

  case class PreprocessingException(msg: String) extends Exception(msg)

  val PrefixOfLhsAuxiliaryPreds = "lhs"
  val PrefixOfRhsAuxiliaryPreds = "rhs"
  val PrefixOfUnfoldingAuxiliaryPreds = "unf"
  private val prefixes = Set(PrefixOfLhsAuxiliaryPreds, PrefixOfRhsAuxiliaryPreds, PrefixOfUnfoldingAuxiliaryPreds)

  def isAuxiliaryPred(pred: Predicate): Boolean = prefixes.contains(pred.head.take(3))

}
