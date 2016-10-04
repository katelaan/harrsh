package slex.algs

import slex.slsyntax.{Emp, IxEq, IxLEq, IxLSeg, IxLT, LSeg, Minus, NullPtr, PointsTo, PtrEq, PtrExpr, PtrNEq, PtrVar, PureAnd, PureFormula, PureNeg, PureOr, SepCon, SepLogFormula, SpatialAtom, SymbolicHeap, True}
import slex.Sorts._
import slex.Combinators._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.annotation.tailrec

/**
  * Model-driven entailment checker
  * FIXME I'm currently not convinced the non-symmetrical subtraction operator is correct. E.g. the entailment ls(x,y,n) /\ Pi |= ls(x,z,l) * ls(z,y,m) /\ Pi' should hold (where z,l,m are implicitly existentially). Or do we disallow existential quantification over index variables? But then the fragment would be entirely useless? Apparently the problem is that lseg describes acyclic list segments, so the entailment indeed does not hold. (The right hand side might describe two acyclic list segments which combined yield a cyclic list; see example below Proposition 2). If we allowed cyclicity, we would indeed need symmetrical subtraction!
  * TODO Can be extended to array separation logic in a straightforward manner
  * TODO Extension to byte-precise separation logic with block predicates?
  * TODO Do we want special treatment of nil? Currently there is none, so we would have to add ∗ next(nil, nil) to regain it
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

  @tailrec
  private def proofLoop(sigmaL : Seq[SpatialAtom], piL : PureFormula, sigmaR : Seq[SpatialAtom], piR : PureFormula, delta : AllocTemplate)(gamma : PureFormula) : VerificationResult = {
    val stackModel = findStackModel(gamma)
    stackModel match {
      case None => Right(Unit) // TODO: What to return in successful case?
      case Some(stack) =>
        val M = matchSHs(stack, delta, sigmaL, sigmaR)
        if (!Evaluator.eval(stack, M))
          Left(M)
        else {
          val extended = PureAnd(gamma, PureNeg(M))
          proofLoop(sigmaL, piL, sigmaR, piR, delta)(extended)
        }
    }
  }

  /**
    * Maatching-based entailment proof under the given stack model
    * @param s Stack evaluation (model returned by SMT solver)
    * @param delta Describes which locations must be allocated
    * @param left Left-hand side of the entailment
    * @param right Right-hand side of the entailment
    * @return
    */
  private def matchSHs(s : Stack, delta : AllocTemplate, left : Seq[SpatialAtom], right : Seq[SpatialAtom]) : PureFormula = {
    // Try to remove an empty formula on the left
    removeEmpty(s, left) match {
      case Some((leftRem, pure)) =>
        PureAnd(pure, matchSHs(s, delta, leftRem, right))
      case None =>

        // Try to remove an empty formula on the right
        removeEmpty(s, right) match {
          case Some((rightRem, pure)) =>
            PureAnd(pure, matchSHs(s, delta, left, rightRem))
          case None =>
            // Currently everything is non-empty. (Might change again through subtraction.)
            // Try to find a matching between non-empty parts

            findMatchingPair(s, left, right) match {
              case Some((leftElem, leftRem, rightElem, rightRem)) =>
                // TODO: Shouldn't we also support the symmetrical case where there is only a partial match on the left? Answer: Only if we supported acyclic list segments -- see also comment near the top
                // Subtraction returns the left-over part of the right matching partner, plus a pure formula expressing the additional constraint
                val (partial, pure) = subtract(delta, rightElem, leftElem)
                val condition = PureAnd(sound(partial), pure)
                if (Evaluator.eval(s, condition)) {
                  val conjunct : PureFormula = PureAnd(PureNeg(separated(leftElem, rightElem)), condition)
                  // Since it might have been a partial match, we might have to add a remainder to the right side of the entailment
                  // TODO We could check for emptiness here to improve efficiency. That would allow us to get rid of all empty formulas in advance. In addition, it would then also make sense to keep the non-empty parts ordered for more efficient matching
                  val newRightSide : Seq[SpatialAtom] = Seq(partial) ++ rightRem
                  PureAnd(conjunct, matchSHs(s, delta, leftRem, newRightSide))
                } else {
                  // The condition is a necessary condition for the entailment, so we can abort here
                  // TODO Verify that this is true (compare Proposition 2)
                  PureNeg(True())
                }

              case None =>
                // No matching pair found =>
                // Entailment only holds if we've already matched everything
                // TODO This is only true in a strict semantics. If we want to support an intuistionistic semantics here, only right needs to empty at the endof the matching?
                if (left.isEmpty && right.isEmpty) True() else PureNeg(True())
            }
        }
    }
  }

  private def removeEmpty(s : Stack, spatial : Seq[SpatialAtom]) : Option[(Seq[SpatialAtom], PureFormula)] =
    if (spatial.isEmpty)
      None
    else {
      val (head, tail) = (spatial.head, spatial.tail)
      if (Evaluator.eval(s, empty(head)))
        Some((tail, empty(head)))
      else
        removeEmpty(s, tail) map {
          case (seq, formula) => ((head :: seq.toList), formula)
        }
    }

  // TODO: Could do this more efficiently...
  private def findMatchingPair(s : Stack, left : Seq[SpatialAtom], right : Seq[SpatialAtom]) : Option[(SpatialAtom, Seq[SpatialAtom], SpatialAtom, Seq[SpatialAtom])] = {
    def orderByAddr(spatial : Seq[SpatialAtom]) = spatial.sortBy(evalAddr(s, _))

    // Finds the first matching addresses in the two ordered sequences of spatial atoms
    // The accumulators store the procesed but unmatched parts of the sequences, to include them in the remainder sequence in the result
    @tailrec
    def orderedMatch(remLeft : Seq[SpatialAtom], remRight : Seq[SpatialAtom], accLeft : Seq[SpatialAtom], accRight : Seq[SpatialAtom]) : Option[(SpatialAtom, Seq[SpatialAtom], SpatialAtom, Seq[SpatialAtom])] =
      if (remLeft.isEmpty || remRight.isEmpty)
        None
      else {
        val leftAddr = evalAddr(s, remLeft.head)
        val rightAddr = evalAddr(s, remLeft.head)
        true match {
          case _ if leftAddr == rightAddr =>
            // Found a matching
            Some(remLeft.head, accLeft ++ remLeft.tail, remRight.head, accRight ++ accRight.tail)
          case _ if leftAddr < rightAddr =>
            // The first left address is smaller than all right addresses => Drop it and continued matching
            orderedMatch(remLeft.tail, remRight, accLeft ++ Seq(remLeft.head), accRight)
          case _ if leftAddr > rightAddr =>
            // The first right address is smaller than all left addresses => Drop it and continued matching
            orderedMatch(remLeft, remRight.tail, accLeft, accRight ++ Seq(remRight.head))
        }
      }

    // Order by addresses
    val orderedLeft = orderByAddr(left)
    val orderedRight = orderByAddr(right)
    orderedMatch(orderedLeft, orderedRight, Seq(), Seq())
  }

  private def subtract(delta : AllocTemplate, large : SpatialAtom, small : SpatialAtom) : (SpatialAtom, PureFormula) = (large, small) match {
    case (PointsTo(_, y), PointsTo(_, b)) =>
      (Emp(), PtrEq(y, b))
    case (IxLSeg(_, y, n), PointsTo(_, b)) =>
      (IxLSeg(b, y, Minus(n, 1)), True())
    case (PointsTo(_, y), IxLSeg(_, b, m)) =>
      (Emp(), PureAnd(PtrEq(y, b), IxEq(m, 1)))
    case (IxLSeg(_, y, n), IxLSeg(_, b, m)) =>
      // FIXME I'm currently not convinced by the following condition. Don't we want to say that *b must be allocated if there is a non-empty part left over*? Currently we're saying that the location at the end of the (truly) larger list must be allocated, unless the shorter list has length exactly 1?! Sounds really strange to me
      val pure = PureOr(PtrEq(y, b), PureOr(delta(y), IxEq(m, 1)))
      // val pure = PureOr(PtrEq(y, b), delta(b))) // FIXME this seems to make more sense to me
      (IxLSeg(b, y, Minus(n, m)), pure)
    case p =>
      throw new Throwable("Subtraction mechanism not defined on pair " + p)
  }

  // TODO This extracts all empty parts at the same time -- keep this for possible reimplementation that does not introduce new predicates with empty interpretation
  //    def extractEmpty(spatial : List[SpatialAtom]) : (List[SpatialAtom], PureFormula) = {
  //      val init : (List[SpatialAtom], PureFormula) = (List(), True())
  //
  //      spatial.foldRight(init){
  //        case (sig, (nonempty,phi)) =>
  //          if (Evaluator.eval(s, empty(sig)))
  //            (nonempty,PureAnd(empty(sig), phi))
  //          else
  //            (sig :: nonempty, phi)
  //      }
  //    }

  def evalAddr(s : Stack, sig : SpatialAtom) : Location = addr(sig) match {
    case NullPtr() => 0
    case p : PtrVar => s(p)
  }

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
