package slex

/**
  * Created by jkatelaa on 10/3/16.
  */
object Combinators {

  def iteratedBinOp[A](binop : (A, A) => A, zero : A)(ops: Seq[A]) : A =
    if (ops.isEmpty) zero else ops.tail.foldLeft(ops.head)(binop)

}
