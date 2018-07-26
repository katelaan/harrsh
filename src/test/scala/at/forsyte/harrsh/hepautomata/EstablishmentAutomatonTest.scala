package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.heapautomata.instances.EstablishmentAutomaton
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomatonTest extends HarrshTableTest with TestValues {

  val est = new EstablishmentAutomaton(acceptEstablished = true)

  val transitions = Table(
    ("src", "sh", "trg est"),
    // Simple RSHs
    (Seq(), SymbolicHeap(), true),
    (Seq(), SymbolicHeap(x1 -> x2), true),
    (Seq(), SymbolicHeap(x1 -> x2, x2 -> y1), false),
    (Seq(), SymbolicHeap(x1 =:= x2, y1 -> x2), true),

    // Inconsistent RSHs
    (Seq(), SymbolicHeap(x1 -> nil, x1 -> nil), true),
    (Seq(), SymbolicHeap(x1 =:= x2, x1 -> nil, x2 -> nil), true),
    (Seq(), SymbolicHeap(x1 =:= x2, x2 =:= x3, x1 =/= x3), true),

    // Non-reduced SHs
    (Seq(TI(Fvs(x1), Alloc(x1), false)), SymbolicHeap(x2 -> nil, P("dummy")(x1)), false),
    (Seq(TI(Fvs(x1), Alloc(x1), true)), SymbolicHeap(x2 -> nil, P("dummy")(x1)), true),
    (Seq(TI(Fvs(x1), Alloc(x1), true)), SymbolicHeap(x2 -> nil, P("dummy")(y1)), true),
    (Seq(TI(Fvs(x1,x2), Eqs(x1 =:= x2), true)), SymbolicHeap(P("dummy")(x1, y1)), true),
    (Seq(TI(Fvs(x1,x2), Eqs(x1 =:= x2), true)), SymbolicHeap(P("dummy")(y2, y1)), false),
    (Seq(TI(Fvs(x1,x2), Eqs(x1 =:= x2), true)), SymbolicHeap(y2 -> x1, P("dummy")(y2, y1)), true),
    (Seq(TI(Fvs(x1,x2), Alloc(x1), Eqs(x1 =:= x2), true)), SymbolicHeap(y2 -> x1, P("dummy")(y2, y1)), true),
    (Seq(TI(Fvs(x1,x2), Eqs(x1 =:= x2), true)), SymbolicHeap(x1 =:= y1, y2 -> x1, P("dummy")(y2, y1)), true)
  )


  property("Transitions of the establishment automaton") {

    forAll(transitions) {
      (src: Seq[est.State], sh: SymbolicHeap, established: Boolean) =>
        Given(src.mkString(", ") + ", " + sh)
        Then(sh + " should be " + (if (established) "ESTABLISHED" else "UNESTABLISHED"))
        val targets = est.getTargetsFor(src, sh)
        targets.size should be (1)
        info("Target state is: " + targets.head)
        targets.head._2 should be (established)
    }

  }

}
