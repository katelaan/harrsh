package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.heapautomata.BaseReachabilityAutomaton._
import at.forsyte.harrsh.heapautomata.{BaseReachabilityAutomaton, TrackingAutomata, _}
import at.forsyte.harrsh.heapautomata.utils.ReachabilityMatrix
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/19/16.
  */
class AcyclicityAutomataTest extends HarrshTableTest {

    def mx3(pairs : (Int,Int)*) : ReachabilityMatrix = ReachabilityMatrix.fromPairs(3, pairs)

    def mk(fvs : Set[FV], pure : Set[PureAtom], mx : ReachabilityMatrix, isAcyclic : Boolean) : (ReachabilityInfo,Boolean) = (((fvs, pure), mx), isAcyclic)
    def mk(fvs : Set[FV], pure : Set[PureAtom], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (((fvs, pure), mx), true)
    def mk(fvs : Set[FV], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (((fvs, Set()), mx), true)

    val WeaklyAcyclic = true
    val Cyclic = false

    val transitions = Table(
      ("src", "sh", "result"),
      // - Simple RSHs
      (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(fv(1), nil))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)), ptr(fv(2), fv(3)))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)), ptr(fv(2), fv(1)))), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)), ptr(fv(2), fv(2)))), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(ptr(fv(1), fv(2)))), Cyclic),

      // - RHSs with free variables
      (Seq(), SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), ptr("y", fv(3))), Seq("y")), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), ptr("y", fv(3)), ptr(fv(3), fv(1))), Seq("y")), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptreq(fv(2), "z")), Seq(ptr(fv(1), "y"), ptr("y", "z"), ptr(fv(2), fv(1))), Seq("y", "z")), Cyclic),

      // - Inconsistent RSHs
      (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)), ptr(fv(1), fv(2)))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptrneq(fv(1), fv(1))), Seq(ptr(fv(1), nil))), WeaklyAcyclic),

      // - Non-R SHs
      (Seq(mk(fvAll(1), mkPure(), mx3(1 -> 1), isAcyclic = false)), SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), call("dummy", fv(2), fv(1)))), Cyclic), // To test propagation of tag bit
      (Seq(mk(fvAll(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")), WeaklyAcyclic),
      (Seq(mk(fvAll(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(fv(1), "z"), call("sll", "z", fv(2))), Seq("z")), WeaklyAcyclic), // To test renaming of fresh var
      (Seq(mk(fvAll(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(fv(2), "y"), call("sll", "y", fv(2))), Seq("y")), Cyclic),
      (Seq(mk(fvAll(1,2), mkPure((1,2,true)), mx3(1 -> 2, 1 -> 3)), mk(fvAll(2,3), mkPure(), mx3(3 -> 2, 2 -> 1))),
        SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), call("dummy", fv(2), "y", "w"), call("dummy", fv(1), fv(3), "w")), Seq("y","w")),
        Cyclic)
    )

    property("Transitions of the acyclicity automaton") {

      val acyc3 = TrackingAutomata.acyclicityAutomaton(3)

      forAll(transitions) {
        (src: Seq[(BaseReachabilityAutomaton.ReachabilityInfo,Boolean)], sh: SymbolicHeap, result: Boolean) =>

          Given(src.mkString(", ") + ", " + sh)
          Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

          println("#"*80)
          val succs = acyc3.getTargetsFor(src, sh)
          println()

          succs.size should be (1)
          info("Reached state: " + succs.head)
          acyc3.isFinal(succs.head) should be (result)
      }

    }

}
