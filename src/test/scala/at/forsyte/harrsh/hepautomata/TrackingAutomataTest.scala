package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.heapautomata.TrackingAutomata
import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.main.FV._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/16/16.
  * Note: This class just tests individual method calls; for complete tests, see HeapAutomataTest
  */
class TrackingAutomataTest extends HarrshTableTest {

  val track3 = TrackingAutomata.singleTargetStateTracking(3, Set(fv(1)), mkPure((2, 3, true)))

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
      SymbolicHeap(Seq(ptr(fv(1), qv(1)), call("sll", qv(1), fv(2)))), // 2nd rule of SLL predicate
      true),
    // - Tree
    (Seq((fvAll(1), mkPure()), (fvAll(1), mkPure())),
      (fvAll(1), mkPure()), // Note: All there is to know is that the parameter is allocated
      SymbolicHeap(Seq(ptr(fv(1), qv(1), qv(2)), call("tree", qv(1)), call("tree", qv(2)))), // 2nd rule of Tree predicate
      true),

    // Testing inconsistency checks for Non-reduced RSHs
    (Seq((fvAll(1), mkPure())), track3.InconsistentState, SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(1), nil))), true),
    (Seq((fvAll(1), mkPure()),(fvAll(), mkPure((1,2,true)))), track3.InconsistentState, SymbolicHeap(Seq(call("sll", fv(1), fv(2)), call("sll", fv(2), fv(3)))), false),
    (Seq((fvAll(1), mkPure()),(fvAll(), mkPure((1,2,true)))), track3.InconsistentState, SymbolicHeap(Seq(call("sll", fv(2), fv(1)), call("sll", fv(2), fv(3)))), false),
    (Seq((fvAll(1), mkPure()),(fvAll(1), mkPure())), track3.InconsistentState, SymbolicHeap(Seq(ptrneq(fv(1),fv(3)), ptrneq(fv(2),fv(3))), Seq(call("sll", fv(3), fv(2)), call("sll", fv(3), fv(1)))), true)
  )

  property("Transitions of the tracking automaton") {

    forAll(transitions) {
      (src: Seq[track3.State], trg: track3.State, sh: SymbolicHeap, result: Boolean) =>
        Given(src.mkString(", ") + ", " + sh + ", " + trg)
        Then(src.mkString(", ") + " --[" + sh + "]--> " + trg + " should be " + (if (result) "DEFINED" else "UNDEFINED"))
        track3.isTransitionDefined(src, trg, sh) should be(result)
    }

  }

//  val sat2 = TrackingAutomata.satAutomaton(2)
//  val srcs = Seq( (fvAll(),mkPure( (0,2,false), (0,1,true), (1,2,false) )), (fvAll(),mkPure( (0,1,true) ))  )
//  val sh = SymbolicHeap(Seq(), Seq(call("succ1circuit", "_x1", "x1"), call("Q", "_x1")), Seq("_x1" ))
//  println(sat2.getTargetsFor(srcs, sh))

}
