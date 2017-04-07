package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.test.HarrshTest

/**
  * Created by jkatelaa on 3/31/17.
  */
class SymbolicHeapTest extends HarrshTest {

  import at.forsyte.harrsh.Implicits._

  val sll = "sll.sid".load()
  val tree = "tree.sid".load()
  val tll = "tll.sid".load()
  val tllAcyc = "tll-acyc.sid".load()

  val tllAcycBaseRule = tllAcyc.baseRule
  val tllAcycRecRule = tllAcyc.recursiveRule

  behavior of "A symbolic heap"

  it should "not have pred calls in reduced heaps" in {

    assert(!"emp".parse().hasPredCalls)
    assert(!"x1 -> y1 * y1 -> y2 : { x1 = y}".parse().hasPredCalls)

  }

  it should "return the correct preds in correct order " in {

    def getCallIds(s : String) : Seq[String] = s.parse().predCalls.map(_.name)

    assert(getCallIds("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1)") == Seq("P","Q"))
    assert(getCallIds("emp * P(x1,y1) * x1 -> y1 * P(x1, y1)") == Seq("P","P"))
    assert(getCallIds("emp * P(x1,y1) * y2 = y3 * x1 -> y1 * P(x1, y1) * R(y1)") == Seq("P","P", "R"))

  }

  it should "return equalities only" in {

    def getEqSet(s : String) : Set[PtrEq] = s.parse().equalities.toSet

    assert(getEqSet("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1)") == Set.empty)
    assert(getEqSet("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 != y1}") == Set.empty)
    assert(getEqSet("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 != y1, x1 != y2}") == Set.empty)
    assert(getEqSet("x1 = y1") != Set.empty)
    assert(getEqSet("x2 = y1") != Set.empty)
    assert(getEqSet("x2 = y1") != getEqSet("x1 = y1"))
    assert(getEqSet("emp : {x1 = y1, x2 = y1}") != Set.empty)
    assert(getEqSet("emp : {x1 = y1, x2 = y1}") == getEqSet("x2 = y1").union(getEqSet("x1 = y1")))
    assert(getEqSet("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 = y1, x1 != y2}") == getEqSet("x1 = y1"))
    assert(getEqSet("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 = y1, x1 != y2}") == getEqSet("x1 = y1"))

  }

//  val unfoldingInstances = Table(
//    ("heap", "bodies", "result"),
//    (TllAcyc.callToStartPred, Seq(tllAcycBaseRule), tllAcycBaseRule),
//    (TllAcyc.callToStartPred, Seq(tllAcycRecRule), tllAcycRecRule),
//    (tllAcycRecRule, Seq(tllAcycBaseRule,tllAcycBaseRule), unfoldedTwiceBase)
//  )


//  println("Checking correctness of call unfolding...")

//  // Depth 1 unfoldings
//  println("Depth 1")
//  println(tllAcycBaseRule)
//  TllAcyc.callToStartPred.replaceCalls(Seq(tllAcycBaseRule), true) should equal(tllAcycBaseRule)
//  println(tllAcycRecRule)
//  TllAcyc.callToStartPred.replaceCalls(Seq(tllAcycRecRule), true) should equal(tllAcycRecRule)
//
//  // Depth 2 red unfolding
  //println("Depth 2")
//  val unfoldedTwiceBase = SymbolicHeap(List(PtrNEq(PtrVar(1), PtrVar(3)), PtrNEq(PtrVar(2), PtrVar(3)), PtrEq(PtrVar(-1), PtrVar(2)), PtrNEq(PtrVar(-1), PtrVar(-3)), PtrEq(PtrVar(-2), PtrVar(-3)), PtrNEq(PtrVar(-2), PtrVar(3))), List(PointsTo(PtrVar(1), Seq(PtrVar(-1), PtrVar(-2), NullPtr())), PointsTo(PtrVar(-1), Seq(NullPtr(), NullPtr(), PtrVar(-3))), PointsTo(PtrVar(-2), Seq(NullPtr(), NullPtr(), PtrVar(3)))), List())
//  println(tllAcycRecRule.replaceCalls(Seq(tllAcycBaseRule, tllAcycBaseRule), true))
//  tllAcycRecRule.replaceCalls(Seq(tllAcycBaseRule, tllAcycBaseRule), true) should equal(unfoldedTwiceBase)
  //println(tllAcycRecRule.replaceCalls(Seq(tllAcycRecRule, tllAcycBaseRule), true))

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
