package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.pure.EqualityUtils.mkPure
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/19/16.
  */
class AcyclicityAutomataTest extends HarrshTableTest with TestValues {

    def mx3(pairs : (Int,Int)*) : ReachabilityMatrix = ReachabilityMatrix.fromPairs(3, pairs)

    def mk(fvs : Set[Var], pure : Set[PureAtom], mx : ReachabilityMatrix, isAcyclic : Boolean) : (ReachabilityInfo,Boolean) = (ReachabilityInfo(TrackingInfo(fvs, pure), mx), isAcyclic)
    def mk(fvs : Set[Var], pure : Set[PureAtom], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (ReachabilityInfo(TrackingInfo(fvs, pure), mx), true)
    def mk(fvs : Set[Var], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (ReachabilityInfo(TrackingInfo(fvs, Set()), mx), true)

    val WeaklyAcyclic = true
    val Cyclic = false

    val transitions = Table(
      ("src", "sh", "result"),
      // - Simple RSHs
      (Seq(), SymbolicHeap(Seq(ptr(x1, x2))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(x1, nil))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(x1, x2), ptr(x2, x3))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptr(x1, x2), ptr(x2, x1))), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptr(x1, x2), ptr(x2, x2))), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptreq(x1,x2)), Seq(ptr(x1, x2)), Seq.empty), Cyclic),

      // - RHSs with free variables
      (Seq(), SymbolicHeap(Seq(), Seq(ptr(x1, y1), ptr(y1, x3)), Seq.empty), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(), Seq(ptr(x1, y1), ptr(y1, x3), ptr(x3, x1)), Seq.empty), Cyclic),
      (Seq(), SymbolicHeap(Seq(ptreq(x2, y2)), Seq(ptr(x1, y1), ptr(y1, y2), ptr(x2, x1)), Seq.empty), Cyclic),

      // - Inconsistent RSHs
      (Seq(), SymbolicHeap(Seq(ptr(x1, x2), ptr(x1, x2))), WeaklyAcyclic),
      (Seq(), SymbolicHeap(Seq(ptrneq(x1, x1)), Seq(ptr(x1, nil)), Seq.empty), WeaklyAcyclic),

      // - Non-R SHs
      (Seq(mk(mkAllVar(1), mkPure(), mx3(1 -> 1), isAcyclic = false)), SymbolicHeap(Seq(), Seq(ptr(x1, x2)), Seq(call("dummy", x2, x1))), Cyclic), // To test propagation of tag bit
      (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))), WeaklyAcyclic),
      (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))), WeaklyAcyclic), // To test renaming of fresh var
      (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(), Seq(ptr(x2, y1)), Seq(call("sll", y1, x2))), Cyclic),
      (Seq(mk(mkAllVar(1,2), mkPure((1,2,true)), mx3(1 -> 2, 1 -> 3)), mk(mkAllVar(2,3), mkPure(), mx3(3 -> 2, 2 -> 1))),
        SymbolicHeap(Seq(), Seq(ptr(x1, x2)), Seq(call("dummy", x2, y1, y2), call("dummy", x1, x3, y2))),
        Cyclic)
    )

    property("Transitions of the acyclicity automaton") {

      val acyc3 = TrackingAutomata.weakAcyclicityAutomaton(3)

      forAll(transitions) {
        (src: Seq[(ReachabilityInfo,Boolean)], sh: SymbolicHeap, result: Boolean) =>

          Given(src.mkString(", ") + ", " + sh)
          Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

          val succs = acyc3.getTargetsFor(src, sh)

          succs.size should be (1)
          info("Reached state: " + succs.head)
          acyc3.isFinal(succs.head) should be (result)
      }

    }

}
