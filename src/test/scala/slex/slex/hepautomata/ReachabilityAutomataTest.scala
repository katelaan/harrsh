package slex.slex.hepautomata

import slex.heapautomata.BaseReachabilityAutomaton.ReachabilityInfo
import slex.heapautomata.TrackingAutomata
import slex.heapautomata._
import slex.heapautomata.utils.ReachabilityMatrix
import slex.seplog._
import slex.slex.SlexTableTest

/**
  * Created by jkatelaa on 10/19/16.
  */
class ReachabilityAutomataTest extends SlexTableTest {

  def mx3(pairs : (Int,Int)*) : ReachabilityMatrix = ReachabilityMatrix.fromPairs(3, pairs)

  def mk(fvs : Set[FV], pure : Set[PureAtom], mx : ReachabilityMatrix) : ReachabilityInfo = ((fvs, pure), mx)
  def mk(fvs : Set[FV], mx : ReachabilityMatrix) : ReachabilityInfo = ((fvs, Set()), mx)

  val transitions = Table(
    ("src", "sh", "from", "to", "result"),
    // - Simple RSHs
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), fv(1), fv(2), true),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), nil))), fv(1), nil, true),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), fv(2), fv(1), false),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(ptr(fv(1), fv(2)))), fv(2), fv(1), true),
    (Seq(), SymbolicHeap(Seq(ptrneq(fv(1),fv(2))), Seq(ptr(fv(1), fv(2)))), fv(2), fv(1), false),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)), ptr(fv(2), fv(3)))), fv(1), fv(3), true),

    // - RHSs with free variables
    (Seq(), SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), ptr("y", fv(3))), Seq("y")), fv(1), fv(3), true),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(2), "z")), Seq(ptr(fv(1), "y"), ptr("y", "z"), ptr(fv(2), fv(3))), Seq("y", "z")), fv(1), fv(3), true),

    // - Inconsistent RSH
    (Seq(), SymbolicHeap(Seq(ptrneq(fv(1), fv(1))), Seq(ptr(fv(1), nil))), fv(1), fv(2), true),

    // - Non-R SHs
    (Seq(mk(fvAll(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")), fv(1), fv(2), true),
    (Seq(mk(fvAll(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(fv(1), "z"), call("sll", "z", fv(2))), Seq("z")), fv(1), fv(2), true), // To test renaming of fresh var
    (Seq(mk(fvAll(), mkPure((1,2,true)), mx3()), mk(fvAll(2,3), mkPure(), mx3(3 -> 2, 2 -> 1))), // 1st call : y=x1, 2nd call : x3 -> w -> y
      SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), call("dummy", "y", fv(1), fv(3)), call("dummy", "y", "w", fv(3))), Seq("y","w")),
      fv(3), fv(2), true)
  )

  property("Transitions of the reachability automaton") {

    forAll(transitions) {
      (src: Seq[BaseReachabilityAutomaton.ReachabilityInfo], sh: SymbolicHeap, from : FV, to : FV, result: Boolean) =>
        val reach3 = TrackingAutomata.reachabilityAutomaton(3, from, to)

        Given(src.mkString(", ") + ", " + sh + ", query " + from + " -> " + to)
        Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

        val succs = reach3.getTargetsFor(src map (ri => (ri,())), sh)
        succs.size should be (1)
        info("Reached state: " + succs.head)
        reach3.isFinal(succs.head) should be (result)
    }

  }

}
