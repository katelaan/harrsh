package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/19/16.
  */
class ReachabilityAutomataTest extends HarrshTableTest with TestValues {

  val transitions = Table(
    ("src", "sh", "from", "to", "result"),
    // - Simple RSHs
    (Seq(), SymbolicHeap(x1 -> x2), x1, x2, true),
    (Seq(), SymbolicHeap(x1 -> nil), x1, nil, true),
    (Seq(), SymbolicHeap(x1 -> x2), x2, x1, false),
    (Seq(), SymbolicHeap(x1 =:= x2, x1 -> x2), x2, x1, true),
    (Seq(), SymbolicHeap(x1 =/= x2, x1 -> x2), x2, x1, false),
    (Seq(), SymbolicHeap(x1 -> x2, x2 -> x3), x1, x3, true),

    // - RHSs with free variables
    (Seq(), SymbolicHeap(x1 -> y1, y1 -> x3), x1, x3, true),
    (Seq(), SymbolicHeap(x2 =:= y2, x1 -> y1, y1 -> y2, x2 -> x3), x1, x3, true),

    // - Inconsistent RSH
    (Seq(), SymbolicHeap(x1 =/= x1, x1 -> nil), x1, x2, false),
    (Seq(), SymbolicHeap(x1 =/= x1, x1 -> nil), x1, nil, false),
    (Seq(), SymbolicHeap(x1 =/= x1, x1 -> x2), x1, x2, false),

    // - Non-R SHs
    (Seq(RI(Fvs(x1,x2), Alloc(x1), Reach((x1, x2)))), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)), x1, x2, true),
    (Seq(RI(Fvs(x1,x2), Alloc(x1), Reach((x1, x2)))), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)), x1, x2, true), // To test renaming of fresh var
    (Seq(
      RI(Fvs(x1,x2,x3), Eqs(x1 =:= x2), Reach()),
      RI(Fvs(x1,x2,x3), Alloc(x2,x3), Reach((x3, x2), (x2, x1)))
    ),
      SymbolicHeap(x1 -> x2, P("dummy")(y1, x1, x3), P("dummy")(y1, y2, x3)),
      x3, x2, true)
  )

  property("Transitions of the reachability automaton") {

    forAll(transitions) {
      (src: Seq[ReachabilityInfo], sh: SymbolicHeap, from : Var, to : Var, result: Boolean) =>
        val reach = TrackingAutomata.reachabilityAutomaton(from, to)

        Given(src.mkString(", ") + ", " + sh + ", query " + from + " -> " + to)
        Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

        val succs = reach.getTargetsFor(src, sh)
        succs.size should be (1)
        info("Reached state: " + succs.head)
        reach.isFinal(succs.head) should be (result)
    }

  }

//  val (src: Seq[ReachabilityInfo], sh: SymbolicHeap, from : Var, to : Var, result: Boolean) = (Seq(
//    RI(Fvs(x1,x2,x3), Eqs(x1 =:= x2), Reach()),
//    RI(Fvs(x1,x2,x3), Alloc(x2,x3), Reach((x3, x2), (x2, x1)))
//  ),
//    SymbolicHeap(x1 -> x2, P("dummy")(y1, x1, x3), P("dummy")(y1, y2, x3)),
//    x3, x2, true)
//  val reach = TrackingAutomata.reachabilityAutomaton(from, to)
//  println(reach.getTargetsFor(src, sh))
//  println("***")

}
