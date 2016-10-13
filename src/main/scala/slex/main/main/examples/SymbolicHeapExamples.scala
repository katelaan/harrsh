package slex.main.main.examples

import slex.seplog._

/**
  * Created by jkatelaa on 10/3/16.
  */
object SymbolicHeapExamples {

  lazy val SingleList = LSeg("x", "y").toSymbolicHeap

  lazy val SplitList = Exists("y", SepCon(LSeg("x", "y"), LSeg("y", "z"))).toSymbolicHeap

  lazy val LassoList = Exists("y", And(SepCon(LSeg("x", "y"), LSeg("y", "y")), PtrNEq("x", "y"))).toSymbolicHeap

  // Entailment example from the paper (lseg(p, qj, j) * qj ↦ q * lseg(q, null, ((n-j)-1)) : {i ≈ (j+1)} |= lseg(p, q, i) * lseg(q, null, (n-i))),
  // but with special "sink"/self-cycle constraint for null to enforce proper null treatment.
  lazy val PaperExampleEntailmentLeft = SymbolicHeap( Seq(IxEq("i", Plus("j",1))), Seq(IxLSeg("p", "qj", "j"), PointsTo("qj", "q"), IxLSeg("q", NullPtr(), Minus(Minus("n", "j"), 1)), PointsTo(NullPtr(), NullPtr())))
  lazy val PaperExampleEntailmentRight = SymbolicHeap( Seq(IxLSeg("p", "q", "i"), IxLSeg("q", NullPtr(), Minus("n", "i")), PointsTo(NullPtr(), NullPtr())))

}
