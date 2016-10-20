package slex.hepautomata

import slex.heapautomata._
import slex.seplog._
import slex.seplog.inductive._
import slex.test.SlexTableTest

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomatonTest extends SlexTableTest {

  val est3 = new EstablishmentAutomaton(3, true)

  def mk(fvs : Set[FV], pure : Set[PureAtom], est : Boolean) : est3.State = ((fvs, pure), est)
  def mk(fvs : Set[FV], est : Boolean) : est3.State = ((fvs, Set()), est)

  val transitions = Table(
    ("src", "sh", "trg est"),
    // Simple RSHs
    (Seq(), SymbolicHeap(Seq(emp)), true),
    (Seq(), SymbolicHeap(Seq(), Seq(emp), Seq("y")), false),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), true),
    (Seq(), SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), ptr(fv(2), "y")), Seq("y")), false),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(ptr("y", fv(2))), Seq("y")), true),

    // Inconsistent RSHs
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), nil), ptr(fv(1), nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(ptr(fv(1), nil), ptr(fv(2), nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1),fv(2)), ptreq(fv(2),fv(3)), ptrneq(fv(1),fv(3))), Seq(emp)), true),

    // Non-reduced SHs
    (Seq(mk(fvAll(1), false)), SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(2), nil))), false),
    (Seq(mk(fvAll(1), true)), SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(2), nil))), true),
    (Seq(mk(fvAll(1), true)), SymbolicHeap(Seq(), Seq(call("dummy", "y"), ptr(fv(2), nil)), Seq("y")), true),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", fv(1), "y")), Seq("y")), true),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", "z", "y")), Seq("y","z")), false),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", "z", "y"), ptr("z", fv(1))), Seq("y","z")), true),
    (Seq(mk(fvAll(1), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", "z", "y"), ptr("z", fv(1))), Seq("y","z")), true),
    (Seq(mk(fvAll(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptreq(fv(1), "y")), Seq(call("dummy", "z", "y"), ptr("z", fv(1))), Seq("y","z")), true)
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
