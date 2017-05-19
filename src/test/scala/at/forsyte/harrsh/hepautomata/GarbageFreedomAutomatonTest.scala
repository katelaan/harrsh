package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.{AtomConstructorFunctions, TestValues}
import at.forsyte.harrsh.heapautomata.instances.TrackingAutomata
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.pure.EqualityUtils.mkPure
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/19/16.
  * TODO Reduce code duplication wrt Acyclicity test
  */
class GarbageFreedomAutomatonTest extends HarrshTableTest with TestValues with AtomConstructorFunctions {

  def mx3(pairs: (Int, Int)*): ReachabilityMatrix = ReachabilityMatrix.fromPairs(3, pairs)

  def mk(fvs: Set[Var], pure: Set[PureAtom], mx: ReachabilityMatrix, isAcyclic: Boolean): (ReachabilityInfo, Boolean) = (ReachabilityInfo(TrackingInfo.fromPair(fvs, pure), mx), isAcyclic)

  def mk(fvs: Set[Var], pure: Set[PureAtom], mx: ReachabilityMatrix): (ReachabilityInfo, Boolean) = (ReachabilityInfo(TrackingInfo.fromPair(fvs, pure), mx), true)

  def mk(fvs: Set[Var], mx: ReachabilityMatrix): (ReachabilityInfo, Boolean) = (ReachabilityInfo(TrackingInfo.fromPair(fvs, Set()), mx), true)

  val GarbageFree = true
  val HasGarbage = false

  val transitions = Table(
    ("src", "sh", "result"),
    // - Simple RSHs without bound vars, hence garbage free by definition
    (Seq(), SymbolicHeap(Seq()), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptr(x1, x2))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptr(x1, nil))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptr(x1, x2), ptr(x2, x1))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptreq(x1, x2)), Seq(ptr(x1, x2)), Seq()), GarbageFree),

    // - RHSs with free variables
    (Seq(), SymbolicHeap.fromFullDescription(Seq(), Seq(), Seq(), 0, Seq(y1)), HasGarbage),
    (Seq(), SymbolicHeap(Seq(ptr(y1,y2), ptr(y2, y3), ptr(y3, x1), ptr(x1, x1))), HasGarbage),
    (Seq(), SymbolicHeap( Seq(ptr(x1, y1), ptr(y1, x3))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptreq(x2, y2)), Seq(ptr(x1, y1), ptr(y1, y2), ptr(x2, x1)), Seq()), GarbageFree),

    // - Inconsistent RSHs
    (Seq(), SymbolicHeap(Seq(ptr(x1, x2), ptr(x1, x2))), GarbageFree),
    (Seq(), SymbolicHeap(Seq(ptrneq(x1, x1)), Seq(ptr(y1, x1, nil)), Seq()), GarbageFree),

    // - Non-R SHs
    (Seq(mk(Set(x1), mkPure(), mx3(1 -> 1), isAcyclic = false)), SymbolicHeap(Seq(ptr(x1, x2)), Seq(call("dummy", x2, x1))), HasGarbage), // To test propagation of tag bit
    (Seq(mk(Set(x1), mx3(1 -> 2))), SymbolicHeap(Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))), GarbageFree),
    (Seq(mk(Set(x2), mx3(2 -> 1))), SymbolicHeap(Seq(ptr(x1, x2)), Seq(call("dummy", x1, y1))), HasGarbage),
    (Seq(mk(Set(x1,x2), mkPure((1,2,true)), mx3(1 -> 2, 1 -> 3)), mk(Set(x2,x3), mkPure(), mx3(3 -> 2, 2 -> 1))),
      SymbolicHeap(Seq(ptr(x1, x2)), Seq(call("dummy", x2, y1, y2), call("dummy", x1, x3, y2))),
      GarbageFree),
    (Seq(mk(Set(x1), mkPure(), mx3(1 -> 2, 1 -> 3)), mk(Set(x1,x2), mkPure((1,2,true)), mx3(1 -> 3, 2 -> 3))),
      SymbolicHeap(Seq(ptr(y2, y3)), Seq(call("dummy", y1, y2, y3), call("dummy", x1, x3, y2))),
      HasGarbage)
  )

  property("Transitions of the garbage-freedom automaton") {

    val garb3 = TrackingAutomata.garbageFreedomAutomaton(3)

    forAll(transitions) {
      (src: Seq[(ReachabilityInfo, Boolean)], sh: SymbolicHeap, result: Boolean) =>

        Given(src.mkString(", ") + ", " + sh)
        Then("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should lead to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))

        val succs = garb3.getTargetsFor(src, sh)

        succs.size should be(1)
        info("Reached state: " + succs.head)
        garb3.isFinal(succs.head) should be(result)
    }

  }

//  val (src, sh, result) = (Seq(mk(mkAllVar(1), mkPure(), mx3(1 -> 2, 1 -> 3)), mk(mkAllVar(1,2), mkPure((1,2,true)), mx3(1 -> 3, 2 -> 3))),
//    SymbolicHeap(Seq(ptr(y2, y3)), Seq(call("dummy", y1, y2, y3), call("dummy", x1, x3, y2))),
//    HasGarbage)
//
//  println(src.mkString(", ") + ", " + sh)
//  println("The transition " + src.mkString(", ") + " --[" + sh + "]--> " + " <trg> should lead to a " + (if (result) "FINAL STATE" else "NON-FINAL STATE"))
//
//  println("#" * 80)
//  val garb3 = TrackingAutomata.garbageFreedomAutomaton(3)
//  val succs = garb3.getTargetsFor(src, sh)
//  println()
//
//  succs.size should be(1)
//  println("Reached state: " + succs.head)
//  garb3.isFinal(succs.head) should be(result)

}