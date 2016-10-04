package slex.main.main.examples

import slex.slsyntax._

/**
  * Created by jkatelaa on 10/3/16.
  */
object SymbolicHeapExamples {

  lazy val SingleList = LSeg("x", "y").toSymbolicHeap

  lazy val SplitList = Exists("y", SepCon(LSeg("x", "y"), LSeg("y", "z"))).toSymbolicHeap

  lazy val LassoList = Exists("y", And(SepCon(LSeg("x", "y"), LSeg("y", "y")), PtrNEq("x", "y"))).toSymbolicHeap

  lazy val Entailment1Left = And(IxEq("i", Plus("iX",1)),
    SepCon(IxLSeg("p", "qX", "iX"),
      SepCon(PointsTo("qX", "q"),
             IxLSeg("q", NullPtr(), Minus(Minus("n", "iX"), 1))))).toSymbolicHeap

  lazy val Entailment1Right = SepCon(IxLSeg("p", "q", "i"),
                                     IxLSeg("q", NullPtr(), Minus("n", "i"))).toSymbolicHeap

}
