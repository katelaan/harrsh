package at.forsyte.harrsh.hepautomata

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
class EstablishmentAutomatonTest extends HarrshTableTest {

  val est3 = new EstablishmentAutomaton(3, true)

  def mk(fvs : Set[Var], pure : Set[PureAtom], est : Boolean) : est3.State = (TrackingInfo.fromPair(fvs, pure), est)
  def mk(fvs : Set[Var], est : Boolean) : est3.State = (TrackingInfo.fromPair(fvs, Set()), est)

  val transitions = Table(
    ("src", "sh", "trg est"),
    // Simple RSHs
    (Seq(), SymbolicHeap(Seq()), true),
    (Seq(), SymbolicHeap.fromFullDescription(Seq(), Seq(), Seq(), 0, Seq(-1)), false),
    (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)))), true),
    (Seq(), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), mkVar(2)), ptr(mkVar(2), qv(1))), Seq()), false),
    (Seq(), SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(ptr(qv(1), mkVar(2))), Seq()), true),

    // Inconsistent RSHs
    (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), nil), ptr(mkVar(1), nil))), true),
    (Seq(), SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2))), Seq(ptr(mkVar(1), nil), ptr(mkVar(2), nil)), Seq()), true),
    (Seq(), SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2)), ptreq(mkVar(2),mkVar(3)), ptrneq(mkVar(1),mkVar(3))), Seq(), Seq()), true),

    // Non-reduced SHs
    (Seq(mk(mkAllVar(1), false)), SymbolicHeap(Seq(ptr(mkVar(2), nil)), Seq(call("dummy", mkVar(1)))), false),
    (Seq(mk(mkAllVar(1), true)), SymbolicHeap(Seq(ptr(mkVar(2), nil)), Seq(call("dummy", mkVar(1)))), true),
    (Seq(mk(mkAllVar(1), true)), SymbolicHeap(Seq(ptr(mkVar(2), nil)), Seq(call("dummy", qv(1)))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", mkVar(1), qv(1)))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(), Seq(call("dummy", qv(2), qv(1)))), false),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptr(qv(2), mkVar(1))), Seq(call("dummy", qv(2), qv(1)))), true),
    (Seq(mk(mkAllVar(1), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptr(qv(2), mkVar(1))), Seq(call("dummy", qv(2), qv(1)))), true),
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), true)), SymbolicHeap(Seq(ptreq(mkVar(1), qv(1))), Seq(ptr(qv(2), mkVar(1))), Seq(call("dummy", qv(2), qv(1)))), true)
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
