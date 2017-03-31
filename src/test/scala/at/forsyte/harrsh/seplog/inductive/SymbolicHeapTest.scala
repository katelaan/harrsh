package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.ExampleSIDs._
import at.forsyte.harrsh.seplog.{NullPtr, PtrVar}
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.test.{HarrshTest}

/**
  * Created by jkatelaa on 3/31/17.
  */
class SymbolicHeapTest extends HarrshTest {

  val tllAcycBaseRule = SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2)), ptrneq(mkVar(1), mkVar(3))), Seq(ptr(mkVar(1),nil,nil,mkVar(3))), Seq.empty)
  val tllAcycRecRule = SymbolicHeap(Seq(ptrneq(mkVar(1), mkVar(3)), ptrneq(mkVar(2), mkVar(3))), Seq(ptr(mkVar(1),qv(1),qv(2),nil)), Seq(call("tll", qv(1), mkVar(2), qv(3)), call("tll", qv(2), qv(3), mkVar(3))))

//  val unfoldingInstances = Table(
//    ("heap", "bodies", "result"),
//    (TllAcyc.callToStartPred, Seq(tllAcycBaseRule), tllAcycBaseRule),
//    (TllAcyc.callToStartPred, Seq(tllAcycRecRule), tllAcycRecRule),
//    (tllAcycRecRule, Seq(tllAcycBaseRule,tllAcycBaseRule), unfoldedTwiceBase)
//  )


  println("Checking correctness of call unfolding...")

//  // Depth 1 unfoldings
//  println("Depth 1")
//  println(tllAcycBaseRule)
//  TllAcyc.callToStartPred.replaceCalls(Seq(tllAcycBaseRule), true) should equal(tllAcycBaseRule)
//  println(tllAcycRecRule)
//  TllAcyc.callToStartPred.replaceCalls(Seq(tllAcycRecRule), true) should equal(tllAcycRecRule)
//
//  // Depth 2 red unfolding
  println("Depth 2")
//  val unfoldedTwiceBase = SymbolicHeap(List(PtrNEq(PtrVar(1), PtrVar(3)), PtrNEq(PtrVar(2), PtrVar(3)), PtrEq(PtrVar(-1), PtrVar(2)), PtrNEq(PtrVar(-1), PtrVar(-3)), PtrEq(PtrVar(-2), PtrVar(-3)), PtrNEq(PtrVar(-2), PtrVar(3))), List(PointsTo(PtrVar(1), Seq(PtrVar(-1), PtrVar(-2), NullPtr())), PointsTo(PtrVar(-1), Seq(NullPtr(), NullPtr(), PtrVar(-3))), PointsTo(PtrVar(-2), Seq(NullPtr(), NullPtr(), PtrVar(3)))), List())
//  println(tllAcycRecRule.replaceCalls(Seq(tllAcycBaseRule, tllAcycBaseRule), true))
//  tllAcycRecRule.replaceCalls(Seq(tllAcycBaseRule, tllAcycBaseRule), true) should equal(unfoldedTwiceBase)
  println(tllAcycRecRule.replaceCalls(Seq(tllAcycRecRule, tllAcycBaseRule), true))

  // Depth 3 red unfoldings
  //println("Depth 3")
  //println(tllAcycRecRule.replaceCalls(Seq(tllAcycRecRule, tllAcycBaseRule), true)).replaceCalls(Seq(tllAcycBaseRule, tllAcycBaseRule), true))
//  println(tllAcycRecRule.replaceCalls(Seq(tllAcycBaseRule, tllAcycRecRule), true).replaceCalls(Seq(tllAcycBaseRule, tllAcycBaseRule), true))
//  println(tllAcycRecRule.replaceCalls(Seq(tllAcycRecRule, tllAcycRecRule), true).replaceCalls(Seq(tllAcycBaseRule, tllAcycBaseRule, tllAcycBaseRule, tllAcycBaseRule), true))


//  property("Call replacement") {
//    forAll(unfoldingInstances) {
//      (heap, bodies, result) =>
//
//        Given(heap + "\n and the bodies " + bodies)
//        Then("The call replacement should return " + result)
//        heap.replaceCalls(bodies, performAlphaConversion = true) should equal(result)
//    }
//  }

}
