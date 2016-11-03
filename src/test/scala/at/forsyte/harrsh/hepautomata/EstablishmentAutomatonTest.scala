package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomatonTest extends HarrshTableTest {

  val est3 = new EstablishmentAutomaton(3, true)

  def mk(fvs : Set[Var], pure : Set[PureAtom], est : Boolean) : est3.State = ((fvs, pure), est)
  def mk(fvs : Set[Var], est : Boolean) : est3.State = ((fvs, Set()), est)

  val transitions = Table(
    ("src", "sh", "trg est"),
    // Simple RSHs
    (Seq(), SymbolicHeap(Seq(emp)), true),
    (Seq(), SymbolicHeap(Seq(), Seq(emp), 0, Seq(-1)), false),
    (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)))), true),
    (Seq(), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), mkVar(2)), ptr(mkVar(2), qv(1)))), false),
    (Seq(), SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(ptr(qv(1), mkVar(2)))), true),

    // Inconsistent RSHs
    (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), nil), ptr(mkVar(1), nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2))), Seq(ptr(mkVar(1), nil), ptr(mkVar(2), nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2)), ptreq(mkVar(2),mkVar(3)), ptrneq(mkVar(1),mkVar(3))), Seq(emp)), true),

    // Non-reduced SHs
    (Seq(mk(mkAllVar(1), false)), SymbolicHeap(Seq(call("dummy", mkVar(1)), ptr(mkVar(2), nil))), false),
    (Seq(mk(mkAllVar(1), true)), SymbolicHeap(Seq(call("dummy", mkVar(1)), ptr(mkVar(2), nil))), true),
    (Seq(mk(mkAllVar(1), true)), SymbolicHeap(Seq(call("dummy", qv(1)), ptr(mkVar(2), nil))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", mkVar(1), qv(1)))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", qv(2), qv(1)))), false),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", qv(2), qv(1)), ptr(qv(2), mkVar(1)))), true),
    (Seq(mk(mkAllVar(1), mkPure((1,2,true)), true)), SymbolicHeap(Seq(call("dummy", qv(2), qv(1)), ptr(qv(2), mkVar(1)))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptreq(mkVar(1), qv(1))), Seq(call("dummy", qv(2), qv(1)), ptr(qv(2), mkVar(1)))), true)
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
