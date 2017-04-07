package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.seplog.{MapBasedRenaming, Renaming, Var}
import at.forsyte.harrsh.test.HarrshTest

/**
  * Created by jkatelaa on 3/31/17.
  */
class SymbolicHeapTest extends HarrshTest with TestValues {

  import at.forsyte.harrsh.Implicits._

  val sll = "sll.sid".load()
  val tree = "tree.sid".load()
  val tll = "tll.sid".load()
  val tllAcyc = "tll-acyc.sid".load()

  val tllAcycBaseRule = tllAcyc.baseRule
  val tllAcycRecRule = tllAcyc.recursiveRule

  behavior of "A symbolic heap"

  it should "not have pred calls in reduced heaps" in {

    println(tllAcyc.callToStartPred.reducedUnfoldings(tllAcyc, 3).mkString("\n"))

    assert(!"emp".parse().hasPredCalls)
    assert(!"x1 -> y1 * y1 -> y2 : { x1 = y}".parse().hasPredCalls)

  }

  it should "return the correct preds in correct order " in {

    def getCallIds(s : String) : Seq[String] = s.parse().identsOfCalledPreds

    assert(getCallIds("emp * x1 = y1 * x1 -> y1 * x2 -> y2") == Seq())
    assert(getCallIds("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1)") == Seq("P","Q"))
    assert(getCallIds("emp * P(x1,y1) * x1 -> y1 * P(x1, y1)") == Seq("P","P"))
    assert(getCallIds("emp * P(x1,y1) * y2 = y3 * x1 -> y1 * P(x1, y1) * R(y1)") == Seq("P","P", "R"))

    assert("emp * P(x1,y1) * y2 = y3 * x1 -> y1 * P(x1, y1) * R(y1)".parse().withoutCalls.identsOfCalledPreds == Seq())

  }

  it should "return all equalities" in {

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

  it should "track free vars correctly" in {

    assert("emp * x6 -> null * Q(x3, x3)".parse().freeVars == Seq(x1,x2,x3,x4,x5,x6)) // Free variables are filled up

    assert(sll.baseRule.body.freeVars == Seq(x1,x2))
    assert(sll.recursiveRule.body.freeVars == Seq(x1,x2))

    for (unf <- tll.callToStartPred.unfoldings(tll,3)) {
      assert(unf.freeVars == Seq(x1,x2,x3))
    }

  }

  it should "have the right vars" in {

    def getVars(s : String) : Set[Var] = s.parse().allVars

    assert(getVars("emp * x1 -> null * Q(y1, y1)") == Set(x1,y1))
    assert(getVars("emp * y6 -> null * Q(y6, y6)") == Set(y1)) // Bound variable names are automatically normalized
    assert(getVars("emp * x1 -> null * Q(x2, x3)") == Set(x1,x2,x3))
    assert(getVars("emp * x6 -> null * Q(x3, x3)") == Set(x1,x2,x3,x4,x5,x6)) // Free variables are filled up
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1)") == Set(x1,y1))
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 != y1}") == Set(x1,y1))
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 != y1, x1 != y2}") == Set(x1,y1,y2))
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, null) : {x1 != null, x1 != y2}") == Set(x1,y1,y2))

  }

  it should "rename vars without double capture" in {

    def renaming(vars : (Var,Var)*) = MapBasedRenaming(Map() ++ vars)

    val testInputs : Seq[(String,Renaming,String)] = Seq(
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> x2), "x2 -> null * Q(y1, y1)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> x2), "x2 -> null * Q(y1, y1)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> x2), "x2 -> null * Q(y1, y1)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> x2, y1 -> x3), "x2 -> null * Q(x3, x3)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> y1), "y1 -> null * Q(y2, y2)"),
      ("x1 -> null * Q(y1, y2) * x2 -> null", renaming(x1 -> y1, x2 -> x1), "y1 -> null * Q(y2, y3) * x1 -> null"),
      // In the following test cases, some or all bound variables are shifted back because of the y in the codomain
      ("x1 -> null * Q(y1, y2) * x2 -> null * R(y3, y4)", renaming(x1 -> x2, x2 -> y1), "x2 -> null * Q(y2, y3) * y1 -> null * R(y4, y5)"),
      ("x1 -> null * Q(y1, y2) * x2 -> null * R(y3, y4)", renaming(x1 -> y3, x2 -> y3), "y3 -> null * Q(y1, y2) * y3 -> null * R(y4, y5)")//,
      // Corner case: Renaming stuff that isn't there
      //("x1 -> null * y1 -> null", renaming(x2 -> y1), "x1 -> null * y1 -> null")
    )

    for {
      (input,ren,output) <- testInputs
    } {
      assert(input.parse().renameVars(ren) == output.parse())
    }

  }

  it should "instantiate vars correctly" in {



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
