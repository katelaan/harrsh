package at.forsyte.harrsh.util

import scala.annotation.tailrec

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

  /**
    * Computes all ordered pairs (a,b) with seq.indexOf(a) < seq.indexOf(b)
    */
  def square[A](seq : Seq[A]) : Seq[(A,A)] = {
    @tailrec
    def aux(seq : Seq[A], acc : Seq[(A,A)]) : Seq[(A,A)] =
      if (seq.isEmpty || seq.tail.isEmpty) acc
      else aux(seq.tail, acc ++ (seq.tail map (rhs => (seq.head, rhs))))

    aux(seq, Seq.empty)
  }


  /**
    * Returns the powerset of the given set
    */
  def powerSet[A](set : Set[A]) : Set[Set[A]] = {
    val seq = set.toSeq

    def aux(elems : Seq[A]) : Set[Set[A]] = {
      if (elems.isEmpty)
        Set(Set())
      else {
        val newelem = elems.head
        val smaller = aux(elems.tail)
        smaller flatMap (set => Set(set, set + newelem))
      }
    }

    aux(seq)
  }

}
