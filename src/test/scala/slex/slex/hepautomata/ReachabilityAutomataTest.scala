package slex.slex.hepautomata

import slex.heapautomata.TrackingAutomata
import slex.heapautomata._
import slex.seplog._
import slex.slex.SlexTableTest

/**
  * Created by jkatelaa on 10/19/16.
  */
class ReachabilityAutomataTest extends SlexTableTest {

  val transitions = Table(
    ("src", "sh", "from", "to", "result"),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), fv(1), fv(2), true),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), fv(2), fv(1), false),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(ptr(fv(1), fv(2)))), fv(2), fv(1), true),
    (Seq(), SymbolicHeap(Seq(ptrneq(fv(1),fv(2))), Seq(ptr(fv(1), fv(2)))), fv(2), fv(1), false),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)), ptr(fv(2), fv(3)))), fv(1), fv(3), true)
  )

  property("Transitions of the reachability automaton") {

    forAll(transitions) {
      (src: Seq[ReachabilityAutomaton.ReachabilityInfo], sh: SymbolicHeap, from : FV, to : FV, result: Boolean) =>
        val reach3 = TrackingAutomata.reachabilityAutomaton(3, from, to)

        Given(src.mkString(", ") + ", " + sh + ", query " + from + " -> " + to)
        Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "TARGET STATE" else "NON-TARGET STATE"))

        val succs = reach3.getTargetsFor(src, sh)
        succs.size should be (1)
        info("Target state: " + succs.head)
        reach3.isFinal(succs.head) should be (result)
    }

  }

}
