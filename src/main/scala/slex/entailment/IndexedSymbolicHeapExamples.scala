package slex.entailment

import slex.seplog.indexed._

/**
  * Created by jkatelaa on 10/3/16.
  */
object IndexedSymbolicHeapExamples {

  lazy val SingleList = IxLSeg("x", "y", "n").toSymbolicHeap

  lazy val SplitList = Exists("y", SepCon(IxLSeg("x", "y", "n"), IxLSeg("y", "z", "m"))).toSymbolicHeap

  // Note: The following is unsound if m>0, because IxLSeg denotes acyclic list segments
  lazy val LassoList = Exists("y", And(SepCon(IxLSeg("x", "y", "n"), IxLSeg("y", "y", "m")), PtrNEq("x", "y"))).toSymbolicHeap

  // Entailment example from the paper (lseg(p, qj, j) * qj ↦ q * lseg(q, null, ((n-j)-1)) : {i ≈ (j+1)} |= lseg(p, q, i) * lseg(q, null, (n-i))),
  // but with special "sink"/self-cycle constraint for null to enforce proper null treatment.
  lazy val PaperExampleEntailmentLeft = IndexedSymbolicHeap( Seq(IxEq("i", Plus("j",1))), Seq(IxLSeg("p", "qj", "j"), ptr("qj", "q"), IxLSeg("q", nil, Minus(Minus("n", "j"), 1)), ptr(nil, nil)))
  lazy val PaperExampleEntailmentRight = IndexedSymbolicHeap( Seq(IxLSeg("p", "q", "i"), IxLSeg("q", nil, Minus("n", "i")), ptr(nil, nil)))

}
