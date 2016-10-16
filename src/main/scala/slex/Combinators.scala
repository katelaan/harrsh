package slex

/**
  * Created by jkatelaa on 10/3/16.
  */
object Combinators {

  def iteratedBinOp[A](binop : (A, A) => A, zero : A)(ops: Seq[A]) : A =
    if (ops.isEmpty) zero else ops.tail.foldLeft(ops.head)(binop)

  def allSeqsOfLength[A](l : Int, as : Set[A]) : Set[Seq[A]]= {
    if (l == 1) as map (Seq(_))
    else {
      val shorter = allSeqsOfLength(l-1, as)
      for {
        a <- as
        seq <- shorter
      } yield a +: seq
    }
  }

  // TODO: Do this more efficiently?
  /**
    * Computes all ordered pairs (a,b) with seq.indexOf(a) < seq.indexOf(b)
    * @param seq
    * @tparam A
    * @return
    */
  def square[A](seq : Seq[A]) : Seq[(A,A)] =
  if (seq.isEmpty || seq.tail.isEmpty) Nil
  else (seq.tail map (rhs => (seq.head, rhs))) ++ square(seq.tail)

}
