package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.{AtomConstructorFunctions, TestValues}
import at.forsyte.harrsh.pure.EqualityUtils.mkPure
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/16/16.
  * Note: This class just tests individual method calls; for complete tests, see HeapAutomataTest
  */
class TrackingAutomataTest extends HarrshTableTest with TestValues with AtomConstructorFunctions {

  val track3 = TrackingAutomata.singleTargetStateTracking(3, Set(x1), mkPure((2, 3, true)))

  val transitions = Table(
    ("src", "trg", "sh", "result"),
    // Simple RSHs
    (Seq(), TrackingInfo.fromPair(Set(x1), mkPure()), SymbolicHeap(Seq(ptr(x1, x2))), true),
    (Seq(), TrackingInfo.fromPair(Set(x2), mkPure()), SymbolicHeap(Seq(ptr(x1, x2))), false),
    (Seq(), TrackingInfo.fromPair(Set(x1,x2), mkPure((1,2,true))), SymbolicHeap(Seq(ptreq(x1, x2)), Seq(ptr(x1, x2)), Seq()), true),

    // RSHs with some propagation
    (Seq(), TrackingInfo.fromPair(Set(x1,x2), mkPure((1,2,false))), SymbolicHeap(Seq(ptr(x1, nil), ptr(x2, nil))), true),

    // Inconsistent RSHs
    (Seq(), track3.inconsistentState, SymbolicHeap(Seq(ptr(x1, nil), ptr(x1, nil))), true),
    (Seq(), track3.inconsistentState, SymbolicHeap(Seq(ptreq(x1,x2)), Seq(ptr(x1, nil), ptr(x2, nil)), Seq()), true),
    (Seq(), track3.inconsistentState, SymbolicHeap(Seq(ptreq(x1,x2), ptreq(x2,x3), ptrneq(x1,x3)), Seq(), Seq()), true),

    // Non-reduced SHs without parameter renaming
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure())), TrackingInfo.fromPair(Set(x1,x2), mkPure((1,2,false))), SymbolicHeap(Seq(ptr(x2, nil)), Seq(call("dummy", x1))), true),
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure()), TrackingInfo.fromPair(Set(), mkPure((1, 2, true)))),
      TrackingInfo.fromPair(Set(x1,x2,x3), mkPure((1,2,true), (1,3,false), (2,3,false))),
      SymbolicHeap(Seq(ptr(x3, nil)), Seq(call("foo", x1), call("bar", x1, x2))),
      true),

    // Non-reducsed SHs with parameter renaming:
    // - SLL
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure((1,2,false)))),
      TrackingInfo.fromPair(Set(x1), mkPure()), // Note: We do not know that x_1 != x_2, but only x_1 != y && x_2 != y; the list may be cyclic
      SymbolicHeap(Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))), // 2nd rule of SLL predicate
      true),
    // - Tree
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure()), TrackingInfo.fromPair(Set(x1), mkPure())),
      TrackingInfo.fromPair(Set(x1), mkPure()), // Note: All there is to know is that the parameter is allocated
      SymbolicHeap(Seq(ptr(x1, y1, y2)), Seq(call("tree", y1), call("tree", y2))), // 2nd rule of Tree predicate
      true),

    // Testing inconsistency checks for Non-reduced RSHs
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure())), track3.inconsistentState, SymbolicHeap(Seq(ptr(x1, nil)), Seq(call("dummy", x1))), true),
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure()),TrackingInfo.fromPair(Set(), mkPure((1,2,true)))), track3.inconsistentState, SymbolicHeap(Seq(), Seq(call("sll", x1, x2), call("sll", x2, x3))), false),
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure()),TrackingInfo.fromPair(Set(), mkPure((1,2,true)))), track3.inconsistentState, SymbolicHeap(Seq(), Seq(call("sll", x2, x1), call("sll", x2, x3))), false),
    (Seq(TrackingInfo.fromPair(Set(x1), mkPure()),TrackingInfo.fromPair(Set(x1), mkPure())), track3.inconsistentState, SymbolicHeap(Seq(ptrneq(x1,x3), ptrneq(x2,x3)), Seq(), Seq(call("sll", x3, x2), call("sll", x3, x1))), true)
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
