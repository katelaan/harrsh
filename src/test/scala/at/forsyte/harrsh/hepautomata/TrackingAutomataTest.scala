package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/16/16.
  * Note: This class just tests individual method calls; for complete tests, see HeapAutomataTest
  */
class TrackingAutomataTest extends HarrshTableTest with TestValues {

  val track = TrackingAutomata.singleTargetStateTracking(Set(x1), Set(x2 =:= x3))

  val transitions = Table(
    ("src", "trg", "sh", "result"),
    // Simple RSHs
    (Seq(), TI(Fvs(x1,x2), Alloc(x1)), SymbolicHeap(x1 -> x2), true),
    (Seq(), TI(Fvs(x1,x2), Alloc(x2)), SymbolicHeap(x1 -> x2), false),
    (Seq(), TI(Fvs(x1,x2), Alloc(x1,x2), Eqs(x1 =:= x2)), SymbolicHeap(x1 =:= x2, x1 -> x2), true),

    // RSHs with some propagation
    (Seq(), TI(Fvs(x1,x2), Alloc(x1,x2), Eqs(x1 =/= x2)), SymbolicHeap(x1 -> nil, x2 -> nil), true),

    // Inconsistent RSHs
    (Seq(), track.inconsistentState(Seq(x1)), SymbolicHeap(x1 -> nil, x1 -> nil), true),
    (Seq(), track.inconsistentState(Seq(x1,x2)), SymbolicHeap(x1 =:= x2, x1 -> nil, x2 -> nil), true),
    (Seq(), track.inconsistentState(Seq(x1,x2,x3)), SymbolicHeap(x1 =:= x2, x2 =:= x3, x1 =/= x3), true),

    // Non-reduced SHs without parameter renaming
    (Seq(TI(Fvs(x1), Alloc(x1))), TI(Fvs(x1,x2), Alloc(x1,x2), Eqs(x1 =/= x2)), SymbolicHeap(x2 -> nil, P("dummy")(x1)), true),
    (Seq(TI(Fvs(x1), Alloc(x1)), TI(Fvs(x1,x2), Eqs(x1 =:= x2))),
      TI(Fvs(x1,x2,x3), Alloc(x1,x2,x3), Eqs(x1 =:= x2, x1 =/= x3, x2 =/= x3)),
      SymbolicHeap(x3 -> nil, P("foo")(x1), P("bar")(x1, x2)),
      true),

    // Non-reducsed SHs with parameter renaming:
    // - SLL
    (Seq(TI(Fvs(x1,x2), Alloc(x1), Eqs(x1 =/= x2))),
      TI(Fvs(x1,x2), Alloc(x1)), // Note: We do not know that x_1 != x_2, but only x_1 != y && x_2 != y; the list may be cyclic
      SymbolicHeap(x1 -> y1, P("sll")(y1, x2)), // 2nd rule of SLL predicate
      true),
    // - Tree
    (Seq(TI(Fvs(x1), Alloc(x1)), TI(Fvs(x1), Alloc(x1))),
      TI(Fvs(x1), Alloc(x1)), // Note: All there is to know is that the parameter is allocated
      SymbolicHeap(x1 -> (y1, y2), P("tree")(y1), P("tree")(y2)), // 2nd rule of Tree predicate
      true),

    // Testing inconsistency checks for Non-reduced RSHs
    (Seq(TI(Fvs(x1), Alloc(x1))), track.inconsistentState(Seq(x1)), SymbolicHeap(x1 -> nil, P("dummy")(x1)), true),
    (Seq(TI(Fvs(x1,x2), Alloc(x1)),TI(Fvs(x1,x2), Eqs(x1 =:= x2))), track.inconsistentState(Seq(x1,x2,x3)), SymbolicHeap(P("sll")(x1, x2), P("sll")(x2, x3)), false),
    (Seq(TI(Fvs(x1,x2), Alloc(x1)),TI(Fvs(x1,x2), Eqs(x1 =:= x2))), track.inconsistentState(Seq(x1,x2,x3)), SymbolicHeap(P("sll")(x2, x1), P("sll")(x2, x3)), false),
    (Seq(TI(Fvs(x1,x2), Alloc(x1)),TI(Fvs(x1,x2), Alloc(x1))), track.inconsistentState(Seq(x1,x2,x3)), SymbolicHeap(x1 =/= x3, x2 =/= x3, P("sll")(x3, x2), P("sll")(x3, x1)), true)
  )

  property("Transitions of the tracking automaton") {

    forAll(transitions) {
      (src: Seq[track.State], trg: track.State, sh: SymbolicHeap, result: Boolean) =>
        Given(src.mkString(", ") + ", " + sh + ", " + trg)
        Then(src.mkString(", ") + " --[" + sh + "]--> " + trg + " should be " + (if (result) "DEFINED" else "UNDEFINED"))
        track.isTransitionDefined(src, trg, sh) should be(result)
    }

  }

//  val sat = TrackingAutomata.satAutomaton
//  val (srcs, trg, sh, res) = (Seq(State(Fvs(x1,x2), Alloc(x1)),State(Fvs(x1,x2), Set(x1 =:= x2))), track.inconsistentState, SymbolicHeap(P("sll")(x1, x2), P("sll")(x2, x3)), false)
//  println(sat.getTargetsFor(srcs, sh))

}
