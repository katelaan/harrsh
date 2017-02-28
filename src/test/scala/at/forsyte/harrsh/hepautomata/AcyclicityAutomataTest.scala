package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.heapautomata.BaseReachabilityAutomaton._
import at.forsyte.harrsh.heapautomata.{BaseReachabilityAutomaton, TrackingAutomata, _}
import at.forsyte.harrsh.heapautomata.utils.ReachabilityMatrix
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/19/16.
  */
class AcyclicityAutomataTest extends HarrshTableTest {

    def mx3(pairs : (Int,Int)*) : ReachabilityMatrix = ReachabilityMatrix.fromPairs(3, pairs)

    def mk(fvs : Set[Var], pure : Set[PureAtom], mx : ReachabilityMatrix, isAcyclic : Boolean) : (ReachabilityInfo,Boolean) = (((fvs, pure), mx), isAcyclic)
    def mk(fvs : Set[Var], pure : Set[PureAtom], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (((fvs, pure), mx), true)
    def mk(fvs : Set[Var], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (((fvs, Set()), mx), true)

    val WeaklyAcyclic = true
    val Cyclic = false

    val transitions = Table(
      ("src", "sh", "result"),
      // - Simple RSHs
      (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), nil))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)), ptr(mkVar(2), mkVar(3)))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)), ptr(mkVar(2), mkVar(1)))), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)), ptr(mkVar(2), mkVar(2)))), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2))), Seq(ptr(mkVar(1), mkVar(2))), Seq.empty), Cyclic),

      // - RHSs with free variables
      (Seq(), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1)), ptr(qv(1), mkVar(3))), Seq.empty), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1)), ptr(qv(1), mkVar(3)), ptr(mkVar(3), mkVar(1))), Seq.empty), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptreq(mkVar(2), qv(2))), Seq(ptr(mkVar(1), qv(1)), ptr(qv(1), qv(2)), ptr(mkVar(2), mkVar(1))), Seq.empty), Cyclic),

      // - Inconsistent RSHs
      (Seq(), SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)), ptr(mkVar(1), mkVar(2)))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptrneq(mkVar(1), mkVar(1))), Seq(ptr(mkVar(1), nil)), Seq.empty), WeaklyAcyclic),

      // - Non-R SHs
      (Seq(mk(mkAllVar(1), mkPure(), mx3(1 -> 1), isAcyclic = false)), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), mkVar(2))), Seq(call("dummy", mkVar(2), mkVar(1)))), Cyclic), // To test propagation of tag bit
      (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1))), Seq(call("sll", qv(1), mkVar(2)))), WeaklyAcyclic),
      (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1))), Seq(call("sll", qv(1), mkVar(2)))), WeaklyAcyclic), // To test renaming of fresh var
      (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(mkVar(2), qv(1))), Seq(call("sll", qv(1), mkVar(2)))), Cyclic),
      (Seq(mk(mkAllVar(1,2), mkPure((1,2,true)), mx3(1 -> 2, 1 -> 3)), mk(mkAllVar(2,3), mkPure(), mx3(3 -> 2, 2 -> 1))),
        SymbolicHeap(Seq(), Seq(ptr(mkVar(1), mkVar(2))), Seq(call("dummy", mkVar(2), qv(1), qv(2)), call("dummy", mkVar(1), mkVar(3), qv(2)))),
        Cyclic)
    )

    property("Transitions of the acyclicity automaton") {

      val acyc3 = TrackingAutomata.weakAcyclicityAutomaton(3)

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
