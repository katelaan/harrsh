package at.forsyte.harrsh.util

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/3/16.
  */
object Combinators {

  def swallowExceptions[A](f : A => Unit, debug : Boolean)(a : A) : Unit = {
    try {
      f(a)
    } catch {
      case e: Throwable =>
        println("Terminating with exception: " + e.getMessage)

        if (debug) {
          println("Terminating with " + e.getClass.toString + " (Message: " + e.getMessage + ")")
          throw e
        }
    }
  }

  def exceptionToNone[A](msg:String)(f : => Option[A]) : Option[A] = {
    try {
      f
    } catch {
      case e : Throwable =>
        println(msg + ": " + e.getMessage)
        None
    }
  }

  def fixedPointComputation[A](initial : A, convergenceTest : (A,A) => Boolean, maxIts : Int = Int.MaxValue)(f : (Int,A) => A) : A = {

    @tailrec def fixedPointAux(prev : A, i : Int) : A = {
      val next = f(i,prev)
      if (convergenceTest(prev, next) || i == maxIts) {
        // Reached fixed point or abort criterion
        next
      } else {
        fixedPointAux(next, i+1)
      }
    }

    fixedPointAux(initial, 1)
  }

  def dropFirstMatch[A](ls: Seq[A], p: A => Boolean): Seq[A] = {
    val index = ls.indexWhere(p)  //index is -1 if there is no match
    if (index < 0) {
      ls
    } else if (index == 0) {
      ls.tail
    } else {
      // splitAt keeps the matching element in the second group
      val (a, b) = ls.splitAt(index)
      a ++ b.tail
    }
  }

  def iteratedBinOp[A](binop : (A, A) => A, zero : A)(ops: Seq[A]) : A =
    if (ops.isEmpty) zero else ops.tail.foldLeft(ops.head)(binop)

  /**
    * Generates all sequences of the given length whose elements are from the set as (repetitions allowed)
    * @param length Length of seqs to generate (at least 1)
    * @param as Set of elements to draw from
    * @return All sequences of the given length whose elements are from the set as (repetitions allowed)
    */
  def allSeqsOfLength[A](length : Int, as : Set[A]) : Set[Seq[A]]= {
    if (length == 1) as map (Seq(_))
    else {
      val shorter = allSeqsOfLength(length-1, as)
      for {
        a <- as
        seq <- shorter
      } yield a +: seq
    }
  }

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
    // TODO Use the built-in subsets function instead?

    def powerSet(elems : Seq[A]) : Set[Set[A]] = {
      if (elems.isEmpty)
        Set(Set())
      else {
        val newelem = elems.head
        val smaller = powerSet(elems.tail)
        smaller flatMap (set => Set(set, set + newelem))
      }
    }

    powerSet(set.toSeq)
  }

  /**
    * Returns partitions of the given set
    */
  def partitions[A](set : Set[A]) : Set[(Set[A],Set[A])] = {

    // TODO Base this on the built-in subsets function instead?

    def partitions(elems : Seq[A]) : Set[(Set[A],Set[A])] = {
      if (elems.isEmpty)
        Set((Set.empty[A],Set.empty[A]))
      else {
        val newelem = elems.head
        val smaller = partitions(elems.tail)
        smaller flatMap {
          case (left,right) => Set((left + newelem, right), (left, right + newelem))
        }
      }
    }

    partitions(set.toSeq)
  }

  /**
    * Returns a sequence of all sequences that can be built by picking one option per element of from
    * @param from Sequences of sets of choices; each element of the result will contain one element of each element of from
    * @return Sequence of all sequences that can be built by picking one option per element of from
    */
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

  @tailrec def lazyAny[A](seq : Seq[A], f : A => Boolean) : Boolean = {
    if (seq.isEmpty) false
    else {
      if (f(seq.head)) true else lazyAny(seq.tail, f)
    }
  }

}
