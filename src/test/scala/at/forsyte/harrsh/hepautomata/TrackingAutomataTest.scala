package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.heapautomata.TrackingAutomata
import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.main.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/16/16.
  * Note: This class just tests individual method calls; for complete tests, see HeapAutomataTest
  */
class TrackingAutomataTest extends HarrshTableTest {

  val track3 = TrackingAutomata.singleTargetStateTracking(3, Set(mkVar(1)), mkPure((2, 3, true)))

  val transitions = Table(
    ("src", "trg", "sh", "result"),
    // Simple RSHs
    (Seq(), (mkAllVar(1), mkPure()), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)))), true),
    (Seq(), (mkAllVar(2), mkPure()), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)))), false),
    (Seq(), (mkAllVar(1,2), mkPure((1,2,true))), SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(ptr(mkVar(1), mkVar(2)))), true),

    // RSHs with some propagation
    (Seq(), (mkAllVar(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(ptr(mkVar(1), nil), ptr(mkVar(2), nil))), true),

    // Inconsistent RSHs
    (Seq(), track3.InconsistentState, SymbolicHeap(Seq(ptr(mkVar(1), nil), ptr(mkVar(1), nil))), true),
    (Seq(), track3.InconsistentState, SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2))), Seq(ptr(mkVar(1), nil), ptr(mkVar(2), nil))), true),
    (Seq(), track3.InconsistentState, SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2)), ptreq(mkVar(2),mkVar(3)), ptrneq(mkVar(1),mkVar(3))), Seq(emp)), true),

    // Non-reduced SHs without parameter renaming
    (Seq((mkAllVar(1), mkPure())), (mkAllVar(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(call("dummy", mkVar(1)), ptr(mkVar(2), nil))), true),
    (Seq((mkAllVar(1), mkPure()), (mkAllVar(), mkPure((1, 2, true)))),
      (mkAllVar(1,2,3), mkPure((1,2,true), (1,3,false), (2,3,false))),
      SymbolicHeap(Seq(call("foo", mkVar(1)), call("bar", mkVar(1), mkVar(2)), ptr(mkVar(3), nil))),
      true),

    // Non-reducsed SHs with parameter renaming:
    // - SLL
    (Seq((mkAllVar(1), mkPure((1,2,false)))),
      (mkAllVar(1), mkPure()), // Note: We do not know that x_1 != x_2, but only x_1 != y && x_2 != y; the list may be cyclic
      SymbolicHeap(Seq(ptr(mkVar(1), qv(1)), call("sll", qv(1), mkVar(2)))), // 2nd rule of SLL predicate
      true),
    // - Tree
    (Seq((mkAllVar(1), mkPure()), (mkAllVar(1), mkPure())),
      (mkAllVar(1), mkPure()), // Note: All there is to know is that the parameter is allocated
      SymbolicHeap(Seq(ptr(mkVar(1), qv(1), qv(2)), call("tree", qv(1)), call("tree", qv(2)))), // 2nd rule of Tree predicate
      true),

    // Testing inconsistency checks for Non-reduced RSHs
    (Seq((mkAllVar(1), mkPure())), track3.InconsistentState, SymbolicHeap(Seq(call("dummy", mkVar(1)), ptr(mkVar(1), nil))), true),
    (Seq((mkAllVar(1), mkPure()),(mkAllVar(), mkPure((1,2,true)))), track3.InconsistentState, SymbolicHeap(Seq(call("sll", mkVar(1), mkVar(2)), call("sll", mkVar(2), mkVar(3)))), false),
    (Seq((mkAllVar(1), mkPure()),(mkAllVar(), mkPure((1,2,true)))), track3.InconsistentState, SymbolicHeap(Seq(call("sll", mkVar(2), mkVar(1)), call("sll", mkVar(2), mkVar(3)))), false),
    (Seq((mkAllVar(1), mkPure()),(mkAllVar(1), mkPure())), track3.InconsistentState, SymbolicHeap(Seq(ptrneq(mkVar(1),mkVar(3)), ptrneq(mkVar(2),mkVar(3))), Seq(call("sll", mkVar(3), mkVar(2)), call("sll", mkVar(3), mkVar(1)))), true)
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
