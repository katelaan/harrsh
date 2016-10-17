package slex.slex.entailment

import org.scalatest.prop.TableDrivenPropertyChecks
import slex.SlexTest
import slex.entailment.MDEC
import slex.seplog._
import slex.smtinteraction.{SimpleZ3Wrapper, SmtWrapper}

/**
  * Created by jkatelaa on 10/13/16.
  */
class MDECTest extends SlexTest with TableDrivenPropertyChecks {

  val entailmentPairs = Table(
    ("lhs", "rhs", "expected result"),

    // 0. Empty entailment true |= true
    (SymbolicHeap( Seq(), Seq() ),
      SymbolicHeap( Seq(), Seq() ),
      true),

    // 1. Entailment ( x -> y * y -> z : { x != z, y != z } |= lseg(x, z, 2)
    (SymbolicHeap( Seq(PtrNEq("y", "z"), PtrNEq("x", "z")), Seq(ptr("x", "y"), ptr("y", "z")) ),
    SymbolicHeap( Seq(IxLSeg("x", "z", 2)) ),
    true),

    // 2. Entailment example from the paper (lseg(p, qj, j) * qj ↦ q * lseg(q, null, ((n-j)-1)) : {i ≈ (j+1)} |= lseg(p, q, i) * lseg(q, null, (n-i))), without special null constraint
    // This does NOT hold, because null is an ordinary variable that can occur in the middle of a list; hence if q = null and q on the path from p to qj, we have a cycle.
    (SymbolicHeap( Seq(IxEq("i", Plus("j",1))), Seq(IxLSeg("p", "qj", "j"), ptr("qj", "q"), IxLSeg("q", nil, Minus(Minus("n", "j"), 1)))),
     SymbolicHeap( Seq(IxLSeg("p", "q", "i"), IxLSeg("q", nil, Minus("n", "i")))),
     false),

    // 3. Like the one before, but with special "sink"/self-cycle interpretation of null.
    // This excludes the scenario sketched above, so the entailment holds
    (SymbolicHeap( Seq(IxEq("i", Plus("j",1))), Seq(IxLSeg("p", "qj", "j"), ptr("qj", "q"), IxLSeg("q", nil, Minus(Minus("n", "j"), 1)), ptr(nil, nil))),
      SymbolicHeap( Seq(IxLSeg("p", "q", "i"), IxLSeg("q", nil, Minus("n", "i")), ptr(nil, nil))),
      true),

    // 4. Entailment example from the paper, but without null ; This does not hold, because if q = qj, the rhs is not entailed
    // (This is identical to the test-case two above, but this might change in the future if null treatment changes.)
    (SymbolicHeap( Seq(IxEq("i", Plus("j",1))),
                   Seq(IxLSeg("p", "qj", "j"), ptr("qj", "q"), IxLSeg("q", "r", Minus(Minus("n", "j"), 1))) ),
      SymbolicHeap( Seq(IxLSeg("p", "q", "i"), IxLSeg("q", "r", Minus("n", "i"))) ),
      false),

    // 5. Extended version of paper example, demanding qj != q. Entailment still does not hold, because if j >= 2, then q might be somewhere in the middle of the first list if q = r, i.e., if the second list is empty!
    (SymbolicHeap( Seq(IxEq("i", Plus("j",1)), PtrNEq("qj", "q")),
                   Seq(IxLSeg("p", "qj", "j"), ptr("qj", "q"), IxLSeg("q", "r", Minus(Minus("n", "j"), 1))) ),
      SymbolicHeap( Seq(IxLSeg("p", "q", "i"), IxLSeg("q", "r", Minus("n", "i"))) ),
      false),

    // 6. Further extension of paper example, demanding in addition to the previous example that also q != r
    (SymbolicHeap( Seq(IxEq("i", Plus("j",1)), PtrNEq("qj", "q"), PtrNEq("q", "r")),
                   Seq(IxLSeg("p", "qj", "j"), ptr("qj", "q"), IxLSeg("q", "r", Minus(Minus("n", "j"), 1))) ),
      SymbolicHeap( Seq(IxLSeg("p", "q", "i"), IxLSeg("q", "r", Minus("n", "i"))) ),
      true)
  )

  forAll(entailmentPairs) {
    (lhs : SymbolicHeap, rhs : SymbolicHeap, expectedResult : Boolean) =>

      SmtWrapper.withZ3 { z3 =>
        val res = new MDEC(z3).prove(lhs, rhs)
        res.isRight shouldBe expectedResult
      }


  }

}
