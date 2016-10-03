package slex.algs

import slex.slsyntax.{Emp, IxEq, IxLEq, IxLSeg, IxLT, LSeg, NullPtr, PointsTo, PtrEq, PtrExpr, PtrNEq, PureAnd, PureFormula, PureNeg, PureOr, SepLogFormula, SpatialAtom, SymbolicHeap, True}
import slex.Sorts._
import slex.Combinators._

/**
  * Model-driven entailment checker
  */
case class MDEC(val left : SymbolicHeap, val right : SymbolicHeap) {

  type AllocTemplate = PtrExpr => PureFormula

  type CounterExample = PureFormula
  type ValidityProof = Unit
  type VerificationResult = Either[CounterExample, ValidityProof]

  def checkSat(phi : PureFormula) : Boolean = {
    ???
  }

  def findStackModel(pi: PureFormula) : Option[Stack] = ???

  def prove(left : SymbolicHeap, right : SymbolicHeap) : VerificationResult = {
    val sigmaL = left.spatial
    val piL = SepLogFormula.fromPureAtoms(left.pure)
    val sigmaR = right.spatial
    val piR = SepLogFormula.fromPureAtoms(right.pure)

    val gamma : PureFormula = PureAnd(piL, wellFormed(sigmaL))

    val delta = alloc(sigmaL)_

    val query = PureAnd(gamma, PureNeg(piR))
    if (checkSat(query))
      Left(query)
    else
      proofLoop(sigmaL, piL, sigmaR, piR, delta)(gamma)
  }

  def proofLoop(sigmaL : Seq[SpatialAtom], piL : PureFormula, sigmaR : Seq[SpatialAtom], piR : PureFormula, delta : AllocTemplate)(gamma : PureFormula) : VerificationResult = {
    val stackModel = findStackModel(gamma)
    stackModel match {
      case None => Right(Unit) // TODO: What to return in successful case?
      case Some(stack) =>
        val M = matchSHs(stack, delta, sigmaL, sigmaR)
        if (!Evaluator.eval(stack, M))
          Left(M)
        else
          proofLoop(sigmaL, piL, sigmaR, piR, delta)(PureAnd(gamma, PureNeg(M)))
    }
  }

  def matchSHs(s : Stack, delta : AllocTemplate, left : Seq[SpatialAtom], right : Seq[SpatialAtom]) : PureFormula = ???

  /**
    * Returns the set of addresses that are definitely allocated based on stack interpretation s
    */
//  def alloc(s : Stack, spatial : Seq[SpatialAtom]) : Set[Location] = {
//    val nonEmpty = spatial filterNot (sig => Evaluator.eval(s, empty(sig)))
//    Set() ++ (nonEmpty map (sig => s(addr(sig))))
//  }

  def alloc(spatial : Seq[SpatialAtom])(x : PtrExpr) : PureFormula = {
    val allocs = spatial map (sig => PureAnd(PureNeg(empty(sig)), PtrEq(x, addr(sig))))
    iteratedBinOp[PureFormula](PureOr, PureNeg(True()))(allocs)
  }

  /**
    * An iterated separating conjunction is well-formed if all its atoms are sound and all atoms are pair-wise separated (i.e., atoms' "starting" addresses are pair-wise different)
    */
  def wellFormed(sig : Seq[SpatialAtom]) : PureFormula = {

    // TODO: Do this more efficiently?
    def square[A](seq : Seq[A]) : Seq[(A,A)] =
      if (seq.isEmpty || seq.tail.isEmpty) Nil
      else (seq zip seq.tail) ++ square(seq.tail)

    val sounds = sig map sound
    val seps = square(sig) map { case (x,y) => separated(x,y) }

    val soundness : PureFormula = iteratedBinOp[PureFormula](PureAnd, True())(sounds)
    val separation : PureFormula = iteratedBinOp[PureFormula](PureAnd, True())(seps)

    PureAnd(soundness, separation)
  }

  /**
    * two predicates are separated if either their addresses are distinct or one of the two predicates is empty
    */
  def separated(left : SpatialAtom, right : SpatialAtom) : PureFormula = {
    PureOr(PtrNEq(addr(left), addr(right)),
      PureOr(empty(left), empty(right))
    )
  }

  def addr(sig : SpatialAtom) : PtrExpr = sig match {
    case Emp() => NullPtr()
    case PointsTo(from, to) => from
    case LSeg(from, to) => from
    case IxLSeg(from, to, lngth) => from
  }

  def sound(sig : SpatialAtom) : PureFormula = sig match {
    case Emp() => True()
    case PointsTo(from, to) => True()
    case LSeg(from, to) => True()
    case IxLSeg(x, y, n) =>
      PureAnd(
        IxLEq(0, n),
        PureAnd(
          // Encoding of (x = y iff 0 = n)
          PureOr(PtrEq(x, y), IxLT(0, n)),
          PureOr(PtrNEq(x, y), IxEq(0, n))
        ))
  }

  def empty(sig : SpatialAtom) : PureFormula = sig match {
    case Emp() => True()
    case PointsTo(from, to) => PureNeg(True())
    case LSeg(from, to) => PtrEq(from, to)
    case IxLSeg(from, to, lngth) => PtrEq(from, to)
  }

}
