package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.heapautomata.BaseReachabilityAutomaton._
import at.forsyte.harrsh.heapautomata.{BaseReachabilityAutomaton, TrackingAutomata, _}
import at.forsyte.harrsh.heapautomata.utils.ReachabilityMatrix
import at.forsyte.harrsh.main.FV
import at.forsyte.harrsh.main.FV._
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/19/16.
  * TODO Reduce code duplication wrt Acyclicity test
  */
class GarbageFreedomAutomatonTest extends HarrshTableTest {

  def mx3(pairs: (Int, Int)*): ReachabilityMatrix = ReachabilityMatrix.fromPairs(3, pairs)

  def mk(fvs: Set[FV], pure: Set[PureAtom], mx: ReachabilityMatrix, isAcyclic: Boolean): (ReachabilityInfo, Boolean) = (((fvs, pure), mx), isAcyclic)

  def mk(fvs: Set[FV], pure: Set[PureAtom], mx: ReachabilityMatrix): (ReachabilityInfo, Boolean) = (((fvs, pure), mx), true)

  def mk(fvs: Set[FV], mx: ReachabilityMatrix): (ReachabilityInfo, Boolean) = (((fvs, Set()), mx), true)

  val GarbageFree = true
  val HasGarbage = false

  val transitions = Table(
    ("src", "sh", "result"),
    // - Simple RSHs without bound vars, hence garbage free by definition
    (Seq(), SymbolicHeap(Seq()), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), nil))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptr(fv(1), fv(2)), ptr(fv(2), fv(1)))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(ptr(fv(1), fv(2)))), GarbageFree),

    // - RHSs with free variables
    (Seq(), SymbolicHeap(Seq(), Seq(), 0, Seq(-1)), HasGarbage),
    (Seq(), SymbolicHeap(Seq(ptr(qv(1),qv(2)), ptr(qv(2), qv(3)), ptr(qv(3), fv(1)), ptr(fv(1), fv(1)))), HasGarbage),
    (Seq(), SymbolicHeap( Seq(ptr(fv(1), qv(1)), ptr(qv(1), fv(3)))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptreq(fv(2), qv(2))), Seq(ptr(fv(1), qv(1)), ptr(qv(1), qv(2)), ptr(fv(2), fv(1)))), GarbageFree),

    // - Inconsistent RSHs
    (Seq(), SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), ptr(fv(1), fv(2)))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptrneq(fv(1), fv(1))), Seq(ptr(qv(1), fv(1), nil))), GarbageFree),

    // - Non-R SHs
    (Seq(mk(fvAll(1), mkPure(), mx3(1 -> 1), isAcyclic = false)), SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), call("dummy", fv(2), fv(1)))), HasGarbage), // To test propagation of tag bit
    (Seq(mk(fvAll(1), mx3(1 -> 2))), SymbolicHeap(Seq(ptr(fv(1), qv(1)), call("sll", qv(1), fv(2)))), GarbageFree),
    (Seq(mk(fvAll(2), mx3(2 -> 1))), SymbolicHeap(Seq(ptr(fv(1), fv(2)), call("dummy", fv(1), qv(1)))), HasGarbage),
    (Seq(mk(fvAll(1,2), mkPure((1,2,true)), mx3(1 -> 2, 1 -> 3)), mk(fvAll(2,3), mkPure(), mx3(3 -> 2, 2 -> 1))),
      SymbolicHeap(Seq(), Seq(ptr(fv(1), fv(2)), call("dummy", fv(2), qv(1), qv(2)), call("dummy", fv(1), fv(3), qv(2)))),
      GarbageFree),
    (Seq(mk(fvAll(1), mkPure(), mx3(1 -> 2, 1 -> 3)), mk(fvAll(1,2), mkPure((1,2,true)), mx3(1 -> 3, 2 -> 3))),
      SymbolicHeap(Seq(), Seq(ptr(qv(2), qv(3)), call("dummy", qv(1), qv(2), qv(3)), call("dummy", fv(1), fv(3), qv(2)))),
      HasGarbage)
  )

  property("Transitions of the garbage-freedom automaton") {

    val garb3 = TrackingAutomata.garbageFreedomAutomaton(3)

    forAll(transitions) {
      (src: Seq[(BaseReachabilityAutomaton.ReachabilityInfo, Boolean)], sh: SymbolicHeap, result: Boolean) =>

        Given(src.mkString(", ") + ", " + sh)
        Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should yield to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

        println("#" * 80)
        val succs = garb3.getTargetsFor(src, sh)
        println()

        succs.size should be(1)
        info("Reached state: " + succs.head)
        garb3.isFinal(succs.head) should be(result)
    }

  }

}