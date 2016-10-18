package slex.slex.hepautomata

import org.scalatest.prop.TableDrivenPropertyChecks
import slex.SlexTest
import slex.heapautomata.TrackingAutomata
import slex.heapautomata._
import slex.seplog.{nil, ptr, ptreq, ptrneq, emp, call, SymbolicHeap}

/**
  * Created by jens on 10/16/16.
  * Note: This class just tests individual method calls; for complete tests, see HeapAutomataTest
  */
class TrackingAutomataTest extends SlexTest with TableDrivenPropertyChecks {

  val track3 = TrackingAutomata(3, Set(fv(1)), mkPure((2, 3, true)))

  println("Testing defined-ness of transitions for tracking automaton")

  val transitions = Table(
    ("src", "trg", "sh", "result"),
    // Simple RSHs
    (Seq(), (fvAll(1), mkPure()), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), true),
    (Seq(), (fvAll(2), mkPure()), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), false),
    (Seq(), (fvAll(1,2), mkPure((1,2,true))), SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(ptr(fv(1), fv(2)))), true),

    // RSHs with some propagation
    (Seq(), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(ptr(fv(1), nil), ptr(fv(2), nil))), true),

    // Inconsistent RSHs
    (Seq(), track3.InconsistentState, SymbolicHeap(Seq(ptr(fv(1), nil), ptr(fv(1), nil))), true),
    (Seq(), track3.InconsistentState, SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(ptr(fv(1), nil), ptr(fv(2), nil))), true),
    (Seq(), track3.InconsistentState, SymbolicHeap(Seq(ptreq(fv(1),fv(2)), ptreq(fv(2),fv(3)), ptrneq(fv(1),fv(3))), Seq(emp)), true),

    // Non-reduced SHs without parameter renaming
    (Seq((fvAll(1), mkPure())), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(2), nil))), true),
    (Seq((fvAll(1), mkPure()), (fvAll(), mkPure((1, 2, true)))),
      (fvAll(1,2,3), mkPure((1,2,true), (1,3,false), (2,3,false))),
      SymbolicHeap(Seq(call("foo", fv(1)), call("bar", fv(1), fv(2)), ptr(fv(3), nil))),
      true),

    // Non-reducsed SHs with parameter renaming:
    // - SLL
    (Seq((fvAll(1), mkPure((1,2,false)))),
      (fvAll(1), mkPure()), // Note: We do not know that x_1 != x_2, but only x_1 != y && x_2 != y; the list may be cyclic
      SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")), // 2nd rule of SLL predicate
      true),
    // - Tree
    (Seq((fvAll(1), mkPure()), (fvAll(1), mkPure())),
      (fvAll(1), mkPure()), // Note: All there is to know is that the parameter is allocated
      SymbolicHeap(Seq(), Seq(ptr(fv(1), "y", "z"), call("tree", "y"), call("tree", "z")), Seq("y", "z")), // 2nd rule of Tree predicate
      true),

    // Testing inconsistency checks for Non-reduced RSHs
    (Seq((fvAll(1), mkPure())), track3.InconsistentState, SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(1), nil))), true),
    (Seq((fvAll(1), mkPure()),(fvAll(), mkPure((1,2,true)))), track3.InconsistentState, SymbolicHeap(Seq(call("sll", fv(1), fv(2)), call("sll", fv(2), fv(3)))), false),
    (Seq((fvAll(1), mkPure()),(fvAll(), mkPure((1,2,true)))), track3.InconsistentState, SymbolicHeap(Seq(call("sll", fv(2), fv(1)), call("sll", fv(2), fv(3)))), false),
    (Seq((fvAll(1), mkPure()),(fvAll(1), mkPure())), track3.InconsistentState, SymbolicHeap(Seq(ptrneq(fv(1),fv(3)), ptrneq(fv(2),fv(3))), Seq(call("sll", fv(3), fv(2)), call("sll", fv(3), fv(1)))), true)
  )

  forAll(transitions) {
    (src : Seq[track3.State], trg: track3.State, sh : SymbolicHeap, result : Boolean) =>
      println("Checking transition " + src.mkString(", ") + " --[" + sh + "]--> " + trg + " for expected result: " + result)
      track3.isTransitionDefined(src, trg, sh) should be (result)
  }

}
