package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.{AtomConstructorFunctions, TestValues}
import at.forsyte.harrsh.heapautomata.instances.EstablishmentAutomaton
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.pure.EqualityUtils.mkPure
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/18/16.
  */
class EstablishmentAutomatonTest extends HarrshTableTest with TestValues with AtomConstructorFunctions {

  val est3 = new EstablishmentAutomaton(3, true)

  def mk(fvs : Set[Var], pure : Set[PureAtom], est : Boolean) : est3.State = (TrackingInfo.fromPair(fvs, pure), est)
  def mk(fvs : Set[Var], est : Boolean) : est3.State = (TrackingInfo.fromPair(fvs, Set()), est)

  val transitions = Table(
    ("src", "sh", "trg est"),
    // Simple RSHs
    (Seq(), SymbolicHeap(Seq()), true),
    (Seq(), SymbolicHeap.fromFullDescription(Seq(), Seq(), Seq(), 0, Seq(-1)), false),
    (Seq(), SymbolicHeap(Seq(ptr(x1, x2))), true),
    (Seq(), SymbolicHeap(Seq(), Seq(ptr(x1, x2), ptr(x2, y1)), Seq()), false),
    (Seq(), SymbolicHeap(Seq(ptreq(x1, x2)), Seq(ptr(y1, x2)), Seq()), true),

    // Inconsistent RSHs
    (Seq(), SymbolicHeap(Seq(ptr(x1, nil), ptr(x1, nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(x1,x2)), Seq(ptr(x1, nil), ptr(x2, nil)), Seq()), true),
    (Seq(), SymbolicHeap(Seq(ptreq(x1,x2), ptreq(x2,x3), ptrneq(x1,x3)), Seq(), Seq()), true),

    // Non-reduced SHs
    (Seq(mk(mkAllVar(1), false)), SymbolicHeap(Seq(ptr(x2, nil)), Seq(call("dummy", x1))), false),
    (Seq(mk(mkAllVar(1), true)), SymbolicHeap(Seq(ptr(x2, nil)), Seq(call("dummy", x1))), true),
    (Seq(mk(mkAllVar(1), true)), SymbolicHeap(Seq(ptr(x2, nil)), Seq(call("dummy", y1))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", x1, y1))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", y2, y1))), false),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptr(y2, x1)), Seq(call("dummy", y2, y1))), true),
    (Seq(mk(mkAllVar(1), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptr(y2, x1)), Seq(call("dummy", y2, y1))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptreq(x1, y1)), Seq(ptr(y2, x1)), Seq(call("dummy", y2, y1))), true)
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
