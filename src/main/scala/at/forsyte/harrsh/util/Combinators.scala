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

//  def square[A](seq : Seq[A]) : Seq[(A,A)] =
//  if (seq.isEmpty || seq.tail.isEmpty) Nil
//  else (seq.tail map (rhs => (seq.head, rhs))) ++ square(seq.tail)

  /**
    * Computes all ordered pairs (a,b) with seq.indexOf(a) < seq.indexOf(b)
    *
    */
  def square[A](seq : Seq[A]) : Seq[(A,A)] = {
    squareAux(seq, Seq())
  }

  @tailrec
  private def squareAux[A](seq : Seq[A], acc : Seq[(A,A)]) : Seq[(A,A)] = {
    if (seq.isEmpty || seq.tail.isEmpty) acc else squareAux(seq.tail, (seq.tail map (rhs => (seq.head, rhs))) ++ acc)
  }

  /**
    * Returns the powerset of the given set
    */
  def powerSet[A](set : Set[A]) : Set[Set[A]] = {
    val seq = set.toSeq

    // TODO: Rewrite to tailrec
    def powerSet(elems : Seq[A]) : Set[Set[A]] = {
      if (elems.isEmpty)
        Set(Set())
      else {
        val newelem = elems.head
        val smaller = powerSet(elems.tail)
        smaller flatMap (set => Set(set, set + newelem))
      }
    }

    powerSet(seq)
  }

  def choices[A](from: Seq[Set[A]]) : Seq[Seq[A]] = {

    def prependAll(prep : Set[A], seq : Seq[A]) : Seq[Seq[A]] = (prep map (x => x +: seq)).toSeq

    if (from.isEmpty)
      Seq(Seq())
    else {
      val newchoice = from.head
      val smaller = choices(from.tail)
      (for {
        seq <- smaller
      } yield prependAll(newchoice, seq)).flatten
    }
  }

}
