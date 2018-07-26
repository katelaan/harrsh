package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/19/16.
  */
class AcyclicityAutomataTest extends HarrshTableTest with TestValues {

    def mx(pairs : (Var, Var)*) : ReachabilityMatrix = ReachabilityMatrix.fromPairs(pairs)

    def mk(fvs: Seq[FreeVar], alloc : Set[Var], pure : Set[PureAtom], mx : ReachabilityMatrix, isAcyclic : Boolean) : (ReachabilityInfo,Boolean) = (ReachabilityInfo(TrackingInfo(alloc, pure, fvs), mx), isAcyclic)
    def mk(fvs: Seq[FreeVar], alloc : Set[Var], pure : Set[PureAtom], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (ReachabilityInfo(TrackingInfo(alloc, pure, fvs), mx), true)
    def mk(fvs: Seq[FreeVar], alloc : Set[Var], mx : ReachabilityMatrix) : (ReachabilityInfo,Boolean) = (ReachabilityInfo(TrackingInfo(alloc, Set(), fvs), mx), true)

    val WeaklyAcyclic = true
    val Cyclic = false

    val transitions = Table(
      ("src", "sh", "result"),
      // - Simple RSHs
      (Seq(), SymbolicHeap(x1 -> x2), WeaklyAcyclic),
      (Seq(), SymbolicHeap(x1 -> nil), WeaklyAcyclic),
      (Seq(), SymbolicHeap(x1 -> x2, x2 -> x3), WeaklyAcyclic),
      (Seq(), SymbolicHeap(x1 -> x2, x2 -> x1), Cyclic),
      (Seq(), SymbolicHeap(x1 -> x2, x2 -> x2), Cyclic),
      (Seq(), SymbolicHeap(x1 =:= x2, x1 -> x2), Cyclic),

      // - RHSs with free variables
      (Seq(), SymbolicHeap(x1 -> y1, y1 -> x3), WeaklyAcyclic),
      (Seq(), SymbolicHeap(x1 -> y1, y1 -> x3, x3 -> x1), Cyclic),
      (Seq(), SymbolicHeap(x2 =:= y2, x1 -> y1, y1 -> x2, x2 -> x1), Cyclic),

      // - Inconsistent RSHs
      (Seq(), SymbolicHeap(x1 -> x2, x1 -> x2), WeaklyAcyclic),
      (Seq(), SymbolicHeap(x1 =/= x1, x1 -> nil), WeaklyAcyclic),

      // - Non-R SHs
      (Seq(mk(Seq(x1,x2), Set(x1), Set(), mx((x1, x1)), isAcyclic = false)), SymbolicHeap(x1 -> x2, P("dummy")(x2, x1)), Cyclic), // To test propagation of tag bit
      (Seq(mk(Seq(x1,x2), Set(x1), mx((x1, x2)))), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)), WeaklyAcyclic),
      (Seq(mk(Seq(x1,x2), Set(x1), mx((x1, x2)))), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)), WeaklyAcyclic), // To test renaming of fresh var
      (Seq(mk(Seq(x1,x2), Set(x1), mx((x1, x2)))), SymbolicHeap(x2 -> y1, P("sll")(y1, x2)), Cyclic),
      (Seq(mk(Seq(x1,x2,x3), Set(x1, x2), Set(x1 =:= x2), mx((x1, x2), (x1, x3))), mk(Seq(x1,x2,x3), Set(x2,x3), Set(), mx((x3, x2), (x2, x1)))),
        SymbolicHeap(x1 -> x2, P("dummy")(x2, y1, y2), P("dummy")(x1, x3, y2)),
        Cyclic)
    )

    property("Transitions of the acyclicity automaton") {

      val acyc = TrackingAutomata.weakAcyclicityAutomaton

      forAll(transitions) {
        (src: Seq[(ReachabilityInfo,Boolean)], sh: SymbolicHeap, result: Boolean) =>

          Given(src.mkString(", ") + ", " + sh)
          Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

          val succs = acyc.getTargetsFor(src, sh)

          succs.size should be (1)
          info("Reached state: " + succs.head)
          acyc.isFinal(succs.head) should be (result)
      }

    }

}
