package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/19/16.
  * TODO Reduce code duplication wrt Acyclicity test
  */
class GarbageFreedomAutomatonTest extends HarrshTableTest with TestValues {

  val GarbageFree = true
  val HasGarbage = false

  val transitions = Table(
    ("src", "sh", "result"),
    // - Simple RSHs without bound vars, hence garbage free by definition
    (Seq(), SymbolicHeap(), GarbageFree),
    (Seq(), SymbolicHeap(x1 -> x2), GarbageFree),
    (Seq(), SymbolicHeap(x1 -> nil), GarbageFree),
    (Seq(), SymbolicHeap(x1 -> x2, x2 -> x1), GarbageFree),
    (Seq(), SymbolicHeap(x1 =:= x2, x1 -> x2), GarbageFree),

    // - RHSs with free variables
    (Seq(), SymbolicHeap(y1 -> y2, y2 -> y3, y3 -> x1, x1 -> x1), HasGarbage),
    (Seq(), SymbolicHeap(x1 -> y1, y1 -> x3), GarbageFree),
    (Seq(), SymbolicHeap(x2 =:= y2, x1 -> y1, y1 -> y2, x2 -> x1), GarbageFree),

    // - Inconsistent RSHs
    (Seq(), SymbolicHeap(x1 -> x2, x1 -> x2), GarbageFree),
    (Seq(), SymbolicHeap(x1 =/= x1, y1 -> (x1, nil)), GarbageFree),

    // - Non-R SHs
    (Seq(RI(Fvs(x1, x2), Alloc(x1), Reach((x1, x1)), HasGarbage)), SymbolicHeap(x1 -> x2, P("dummy")(x2, x1)), HasGarbage), // To test propagation of tag bit
    (Seq(RI(Fvs(x1, x2), Alloc(x1), Reach((x1, x2)), GarbageFree)), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)), GarbageFree),
    (Seq(RI(Fvs(x1, x2), Alloc(x2), Reach((x2, x1)), GarbageFree)), SymbolicHeap(x1 -> x2, P("dummy")(x1, y1)), HasGarbage),
    (Seq(
      RI(Fvs(x1,x2,x3), Alloc(x1,x2), Eqs(x1 =:= x2), Reach((x1, x2), (x1, x3)), GarbageFree),
      RI(Fvs(x1,x2,x3), Alloc(x2,x3), Reach((x3, x2), (x2, x1)), GarbageFree)
    ),
      SymbolicHeap(x1 -> x2, P("dummy")(x2, y1, y2), P("dummy")(x1, x3, y2)),
      GarbageFree),
    (Seq(
      RI(Fvs(x1,x2,x3), Alloc(x1), Reach((x1, x2), (x1, x3)), GarbageFree),
      RI(Fvs(x1,x2,x3), Alloc(x1,x2), Eqs(x1 =:= x2), Reach((x1, x3), (x2, x3)), GarbageFree)
    ),
      SymbolicHeap(y2 -> y3, P("dummy")(y1, y2, y3), P("dummy")(x1, x3, y2)),
      HasGarbage)
  )

  property("Transitions of the garbage-freedom automaton") {

    val garb = TrackingAutomata.garbageFreedomAutomaton

    forAll(transitions) {
      (src: Seq[(ReachabilityInfo, Boolean)], sh: SymbolicHeap, result: Boolean) =>

        Given(src.mkString(", ") + ", " + sh)
        Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should lead to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

        val succs = garb.getTargetsFor(src, sh)

        succs.size should be(1)
        info("Reached state: " + succs.head)
        garb.isFinal(succs.head) should be(result)
    }

  }

//  val (src, sh, result) = (Seq(RI(RIAllVar(1), RIPure(), Reach3(1 -> 2, 1 -> 3)), RI(RIAllVar(1,2), RIPure((1,2,true)), Reach3(1 -> 3, 2 -> 3))),
//    SymbolicHeap(Seq(ptr(y2, y3)), Seq(call("dummy", y1, y2, y3), call("dummy", x1, x3, y2))),
//    HasGarbage)
//
//  println(src.RIString(", ") + ", " + sh)
//  println("The transition " + src.RIString(", ") + " --[" + sh + "]--> " + " <trg> should lead to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))
//
//  println("#" * 80)
//  val garb3 = TrackingAutomata.garbageFreedomAutomaton(3)
//  val succs = garb3.getTargetsFor(src, sh)
//  println()
//
//  succs.size should be(1)
//  println("Reached state: " + succs.head)
//  garb3.isFinal(succs.head) should be(result)

}