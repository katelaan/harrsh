package slex.main.main.examples

import slex.slsyntax._

/**
  * Created by jkatelaa on 10/3/16.
  */
object SymbolicHeapExamples {

  lazy val SingleList = LSeg("x", "y").toSymbolicHeap

  lazy val SplitList = Exists("y", SepCon(LSeg("x", "y"), LSeg("y", "z"))).toSymbolicHeap

  lazy val LassoList = Exists("y", And(SepCon(LSeg("x", "y"), LSeg("y", "y")), PtrNEq("x", "y"))).toSymbolicHeap

  // Entailment ( x -> y * y -> z : { x != z } |= lseg(x, z, 2) ; EXPECTED RESULT: false -- lhs might be cyclic, since y = z not excluded
  lazy val Entailment0Left = And(PtrNEq("x", "z"),
    SepCon(PointsTo("x", "y"), PointsTo("y", "z"))).toSymbolicHeap

  lazy val Entailment0Right = IxLSeg("x", "z", 2).toSymbolicHeap
  
  // Entailment ( x -> y * y -> z : { x != z, y != z } |= lseg(x, z, 2) ; EXPECTED RESULT: false -- lhs might be cyclic, since y = z not excluded
  lazy val Entailment1Left = And(PtrNEq("y", "z"), And(PtrNEq("x", "z"),
    SepCon(PointsTo("x", "y"), PointsTo("y", "z")))).toSymbolicHeap

  lazy val Entailment1Right = IxLSeg("x", "z", 2).toSymbolicHeap

  // Entailment example from the paper
  lazy val Entailment2Left = And(IxEq("i", Plus("iX",1)),
    SepCon(IxLSeg("p", "qX", "iX"),
      SepCon(PointsTo("qX", "q"),
             IxLSeg("q", NullPtr(), Minus(Minus("n", "iX"), 1))))).toSymbolicHeap

  lazy val Entailment2Right = SepCon(IxLSeg("p", "q", "i"),
                                     IxLSeg("q", NullPtr(), Minus("n", "i"))).toSymbolicHeap

  // Like the one before, but with special interpretation of null
  lazy val Entailment3Left = And(IxEq("i", Plus("iX",1)),
    SepCon(
      PointsTo(NullPtr(), NullPtr()),
      SepCon(IxLSeg("p", "qX", "iX"),
        SepCon(PointsTo("qX", "q"),
          IxLSeg("q", NullPtr(), Minus(Minus("n", "iX"), 1)))))).toSymbolicHeap

  lazy val Entailment3Right = SepCon(
      PointsTo(NullPtr(), NullPtr()),
      SepCon(IxLSeg("p", "q", "i"),
           IxLSeg("q", NullPtr(), Minus("n", "i")))).toSymbolicHeap

}
