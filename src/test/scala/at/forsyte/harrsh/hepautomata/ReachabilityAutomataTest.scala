package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.{AtomConstructorFunctions, TestValues}
import at.forsyte.harrsh.pure.EqualityUtils.mkPure
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/19/16.
  */
class ReachabilityAutomataTest extends HarrshTableTest with TestValues with AtomConstructorFunctions {

  def mx3(pairs : (Int,Int)*) : ReachabilityMatrix = ReachabilityMatrix.fromPairs(3, pairs)

  def mk(fvs : Set[Var], pure : Set[PureAtom], mx : ReachabilityMatrix) : ReachabilityInfo = ReachabilityInfo(TrackingInfo.fromPair(fvs, pure), mx)
  def mk(fvs : Set[Var], mx : ReachabilityMatrix) : ReachabilityInfo = ReachabilityInfo(TrackingInfo.fromPair(fvs, Set()), mx)

  val transitions = Table(
    ("src", "sh", "from", "to", "result"),
    // - Simple RSHs
    (Seq(), SymbolicHeap(Seq(ptr(x1, x2))), x1, x2, true),
    (Seq(), SymbolicHeap(Seq(ptr(x1, nil))), x1, nil, true),
    (Seq(), SymbolicHeap(Seq(ptr(x1, x2))), x2, x1, false),
    (Seq(), SymbolicHeap(Seq(ptreq(x1,x2)), Seq(ptr(x1, x2)), Seq()), x2, x1, true),
    (Seq(), SymbolicHeap(Seq(ptrneq(x1,x2)), Seq(ptr(x1, x2)), Seq()), x2, x1, false),
    (Seq(), SymbolicHeap(Seq(ptr(x1, x2), ptr(x2, x3)), Seq()), x1, x3, true),

    // - RHSs with free variables
    (Seq(), SymbolicHeap(Seq(ptr(x1, y1), ptr(y1, x3))), x1, x3, true),
    (Seq(), SymbolicHeap(Seq(ptreq(x2, y2)), Seq(ptr(x1, y1), ptr(y1, y2), ptr(x2, x3)), Seq()), x1, x3, true),

    // - Inconsistent RSH
    (Seq(), SymbolicHeap(Seq(ptrneq(x1, x1)), Seq(ptr(x1, nil)), Seq()), x1, x2, true),

    // - Non-R SHs
    (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))), x1, x2, true),
    (Seq(mk(mkAllVar(1), mx3(1 -> 2))), SymbolicHeap(Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))), x1, x2, true), // To test renaming of fresh var
    (Seq(mk(mkAllVar(), mkPure((1,2,true)), mx3()), mk(mkAllVar(2,3), mkPure(), mx3(3 -> 2, 2 -> 1))), // 1st call : y=x1, 2nd call : x3 -> w -> y
      SymbolicHeap(Seq(ptr(x1, x2)), Seq(call("dummy", y1, x1, x3), call("dummy", y1, y2, x3))),
      x3, x2, true)
  )

  property("Transitions of the reachability automaton") {

    forAll(transitions) {
      (src: Seq[ReachabilityInfo], sh: SymbolicHeap, from : Var, to : Var, result: Boolean) =>
        val reach3 = TrackingAutomata.reachabilityAutomaton(3, from, to)

        Given(src.mkString(", ") + ", " + sh + ", query " + from + " -> " + to)
        Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

        val succs = reach3.getTargetsFor(src, sh)
        succs.size should be (1)
        info("Reached state: " + succs.head)
        reach3.isFinal(succs.head) should be (result)
    }

  }

}
