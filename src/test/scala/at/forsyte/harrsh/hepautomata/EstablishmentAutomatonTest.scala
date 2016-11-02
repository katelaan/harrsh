package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.main.FV
import at.forsyte.harrsh.main.FV._
import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomatonTest extends HarrshTableTest {

  val est3 = new EstablishmentAutomaton(3, true)

  def mk(fvs : Set[FV], pure : Set[PureAtom], est : Boolean) : est3.State = ((fvs, pure), est)
  def mk(fvs : Set[FV], est : Boolean) : est3.State = ((fvs, Set()), est)

  val transitions = Table(
    ("src", "sh", "trg est"),
    // Simple RSHs
    (Seq(), SymbolicHeap(Seq(emp)), true),
    (Seq(), SymbolicHeap(Seq(), Seq(emp), 0, Seq(-1)), false),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), true),
    (Seq(), SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), ptr(fv(2), qv(1)))), false),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(ptr(qv(1), fv(2)))), true),

    // Inconsistent RSHs
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), nil), ptr(fv(1), nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(ptr(fv(1), nil), ptr(fv(2), nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1),fv(2)), ptreq(fv(2),fv(3)), ptrneq(fv(1),fv(3))), Seq(emp)), true),

    // Non-reduced SHs
    (Seq(mk(fvAll(1), false)), SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(2), nil))), false),
    (Seq(mk(fvAll(1), true)), SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(2), nil))), true),
    (Seq(mk(fvAll(1), true)), SymbolicHeap(Seq(call("dummy", qv(1)), ptr(fv(2), nil))), true),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", fv(1), qv(1)))), true),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", qv(2), qv(1)))), false),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", qv(2), qv(1)), ptr(qv(2), fv(1)))), true),
    (Seq(mk(fvAll(1), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", qv(2), qv(1)), ptr(qv(2), fv(1)))), true),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptreq(fv(1), qv(1))), Seq(call("dummy", qv(2), qv(1)), ptr(qv(2), fv(1)))), true)
  )


  property("Transitions of the establishment automaton") {

    forAll(transitions) {
      (src: Seq[est3.State], sh: SymbolicHeap, established: Boolean) =>
        Given(src.mkString(", ") + ", " + sh)
        Then(sh + " should be " + (if (established) "ESTABLISHED" else "UNESTABLISHED"))
        val targets = est3.getTargetsFor(src, sh)
        targets.size should be (1)
        info("Target state is: " + targets.head)
        targets.head._2 should be (established)
    }

  }

}
