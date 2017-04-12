package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.seplog.{Renaming, Var}
import at.forsyte.harrsh.test.HarrshTest

/**
  * Created by jkatelaa on 3/31/17.
  */
class SymbolicHeapTest extends HarrshTest with TestValues {

  import at.forsyte.harrsh.Implicits._

  behavior of "A symbolic heap"

  it should "not have pred calls in reduced heaps" in {
    assert(!"emp".parse.hasPredCalls)
    assert(!"x1 -> y1 * y1 -> y2 : { x1 = y}".parse.hasPredCalls)

  }

  it should "return the correct preds in correct order " in {

    def getCallIds(s : String) : Seq[String] = s.parse.identsOfCalledPreds

    assert(getCallIds("emp * x1 = y1 * x1 -> y1 * x2 -> y2") == Seq())
    assert(getCallIds("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1)") == Seq("P","Q"))
    assert(getCallIds("emp * P(x1,y1) * x1 -> y1 * P(x1, y1)") == Seq("P","P"))
    assert(getCallIds("emp * P(x1,y1) * y2 = y3 * x1 -> y1 * P(x1, y1) * R(y1)") == Seq("P","P", "R"))

    assert("emp * P(x1,y1) * y2 = y3 * x1 -> y1 * P(x1, y1) * R(y1)".parse.withoutCalls.identsOfCalledPreds == Seq())

  }

  it should "return all equalities" in {
    
    def getEqSet(s : String) : Set[PtrEq] = s.parse.equalities.toSet

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

    val sll = "sll.sid".load()
    val tll = "tll.sid".load()

    assert("emp * x6 -> null * Q(x3, x3)".parse.freeVars == Seq(x1,x2,x3,x4,x5,x6)) // Free variables are filled up

    assert(sll.baseRule.body.freeVars == Seq(x1,x2))
    assert(sll.recursiveRule.body.freeVars == Seq(x1,x2))

    for (unf <- tll.callToStartPred.unfoldings(tll,3)) {
      assert(unf.freeVars == Seq(x1,x2,x3))
    }

  }

  it should "have the right vars" in {

    def getVars(s : String) : Set[Var] = s.parse.allVars

    assert(getVars("emp * x1 -> null * Q(y1, y1)") == Set(x1,y1))
    assert(getVars("emp * y6 -> null * Q(y6, y6)") == Set(y1)) // Bound variable names are automatically normalized
    assert(getVars("emp * x1 -> null * Q(x2, x3)") == Set(x1,x2,x3))
    assert(getVars("emp * x6 -> null * Q(x3, x3)") == Set(x1,x2,x3,x4,x5,x6)) // Free variables are filled up
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1)") == Set(x1,y1))
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 != y1}") == Set(x1,y1))
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, y1) : {x1 != y1, x1 != y2}") == Set(x1,y1,y2))
    assert(getVars("emp * P(x1,y1) * x1 -> y1 * Q(x1, null) : {x1 != null, x1 != y2}") == Set(x1,y1,y2))

  }

  def renaming(vars : (Var,Var)*) : Renaming = Renaming.fromPairs(vars)

  it should "rename vars without double capture" in {

    val testInputs : Seq[(String,Renaming,String)] = Seq(
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> x2), "x2 -> null * Q(y1, y1)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> x2, y1 -> x3), "x2 -> null * Q(x3, x3)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> y1), "y1 -> null * Q(y2, y2)"),
      // In the following test cases, some or all bound variables are shifted back because of the y in the codomain
      ("x1 -> null * Q(y1, y2) * x2 -> null", renaming(x1 -> y1, x2 -> x1), "y1 -> null * Q(y2, y3) * x1 -> null"),
      ("x1 -> null * Q(y1, y2) * x2 -> null * R(y3, y4)", renaming(x1 -> x2, x2 -> y1), "x2 -> null * Q(y2, y3) * y1 -> null * R(y4, y5)"),
      ("x1 -> null * Q(y1, y2) * x2 -> null * R(y3, y4)", renaming(x1 -> y3, x2 -> y3), "y3 -> null * Q(y1, y2) * y3 -> null * R(y4, y5)")//,
      // Corner case: Renaming stuff that isn't there
      //("x1 -> null * y1 -> null", renaming(x2 -> y1), "x1 -> null * y1 -> null")
    )

    for {
      (input,ren,output) <- testInputs
    } {
      info("Testing equality of renaming with alpha conversion " + input + ren + " == " + output)
      assert(input.parse.renameVars(ren) == output.parse)
    }

  }

  it should "rename vars with double capture" in {

    val testInputs : Seq[(String,Renaming,String)] = Seq(
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> x2), "x2 -> null * Q(y1, y1)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> y1), "y1 -> null * Q(y1, y1)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> y1, y1 -> y2), "y1 -> null * Q(y2, y2)"),
      ("x1 -> null * Q(y1, y1)", renaming(x1 -> y1, y1 -> x3), "y1 -> null * Q(x3, x3)"),
      ("x1 -> null * Q(y1, y2) * x2 -> null", renaming(x1 -> y1, x2 -> x1), "y1 -> null * Q(y1, y2) * x1 -> null"),
      ("x1 -> null * Q(y1, y2) * x2 -> null * R(y3, y4)", renaming(x1 -> x2, x2 -> y1), "x2 -> null * Q(y1, y2) * y1 -> null * R(y3, y4)"),
      ("x1 -> null * Q(y1, y2) * x2 -> null * R(y3, y4)", renaming(x1 -> y3, x2 -> y3), "y3 -> null * Q(y1, y2) * y3 -> null * R(y3, y4)"),
      ("x1 -> null * y1 -> null", renaming(x2 -> y1), "x1 -> null * y1 -> null")//,
      // Renaming bound variables to free variables
      // TODO The following test cases fail because renaming does not clean up gaps in bound vars. For the time being, we leave it like that, since always closing gaps will incur needless computational overheads for some tasks such as model checking. It would be much cleaner, more predictable behavior if we closed such gaps, though, so I'm still not sure what the best option is.
//      ("x1 -> null * Q(y1, y1)", renaming(y1 -> x1), "x1 -> null * Q(x1, x1)"),
//      ("x1 -> null * Q(y1, y1)", renaming(y1 -> x2), "x1 -> null * Q(x2, x2)"),
//      ("x1 -> null * Q(y1, y2) * y1 -> (y2, y3)", renaming(y1 -> x1,y2 -> x2), "x1 -> null * Q(x1, x2) * x1 -> (x2, y1)"),
//      ("x1 -> null * Q(y1, y2) * y1 -> (y2, y3)", renaming(y1 -> x1,y2 -> x5), "x1 -> null * Q(x1, x5) * x1 -> (x5, y1)")
    )

    for {
      (input,ren,output) <- testInputs
    } {
      info("Testing equality of renaming " + input + ren + " == " + output)
      assert(input.parse.renameVars(ren,avoidDoubleCapture = false) == output.parse)
    }

  }

  it should "instantiate a bound vars with a free var" in {

    val testInputs : Seq[(String,(Var,Var),String)] = Seq(
      ("x1 -> y1", (y1,x1), "x1 -> x1"),
      ("x1 -> y1", (y1,x2), "x1 -> x2"),
      ("x1 -> (y2,y1) * y2 -> (x3,y1)", (y2,x2), "x1 -> (x2,y1) * x2 -> (x3,y1)"),
      // In the following, gaps in FV seqs are closed
      ("x1 -> (y1,y2) * y1 -> (x3,y2)", (y1,x2), "x1 -> (x2,y2) * x2 -> (x3,y2)"),
      ("x1 -> (y1,y2) * P(y3,y4,y5)", (y2,x2), "x1 -> (y1,x2) * P(y2,y3,y4)")
    )

    for {
      (input,(qvar,fvar),output) <- testInputs
    } {
      info("Testing equality of instantiation " + input + "[" + qvar + " -> " + fvar + "] == " + output)
      assert(input.parse.instantiateBoundVars(Seq((qvar, fvar)), closeGaps = true) == output.parse)
    }

  }

  it should "replace calls with proper renaming" in {

    val tllCall = "tll(x1, x2, x3)"
    val tllBase = "x1 ↦ (null, null, x3) : {x1 ≈ x2, x1 ≉ x3}"
    val tllRec = "∃y1 ∃y2 ∃y3 . x1 ↦ (y1, y2, null) * tll(y1,x2,y3) * tll(y2,y3,x3) : {x1 ≉ x3, x2 ≉ x3}"
    val tllRec_2Base = "∃y1 ∃y2 ∃y3 . x1 ↦ (y1, y2, null) * y1 ↦ (null, null, y3) * y2 ↦ (null, null, x3) : {x1 ≉ x3, x2 ≉ x3, y1 ≈ x2, y1 ≉ y3, y2 ≈ y3, y2 ≉ x3}"
    val tllRec_RecBase = "∃y1 ∃y2 ∃y3 ∃y4 ∃y5 ∃y6 . x1 ↦ (y1, y2, null) * y1 ↦ (y6, y4, null) * y2 ↦ (null, null, x3) * tll(y6,x2,y5) * tll(y4,y5,y3) : {x1 ≉ x3, x2 ≉ x3, y1 ≉ y3, x2 ≉ y3, y2 ≈ y3, y2 ≉ x3}"
    val tllRec_BaseRec = "∃y1 ∃y2 ∃y3 ∃y4 ∃y5 ∃y6 . x1 ↦ (y1, y2, null) * y1 ↦ (null, null, y3) * y2 ↦ (y6, y4, null) * tll(y6,y3,y5) * tll(y4,y5,x3) : {x1 ≉ x3, x2 ≉ x3, y1 ≈ x2, y1 ≉ y3, y2 ≉ x3, y3 ≉ x3}"
    val tllRec_2Rec = "∃y1 ∃y2 ∃y3 ∃y4 ∃y5 ∃y6 ∃y7 ∃y8 ∃y9 . x1 ↦ (y1, y2, null) * y1 ↦ (y6, y4, null) * y2 ↦ (y9, y7, null) * tll(y6,x2,y5) * tll(y4,y5,y3) * tll(y9,y3,y8) * tll(y7,y8,x3) : {x1 ≉ x3, x2 ≉ x3, y1 ≉ y3, x2 ≉ y3, y2 ≉ x3, y3 ≉ x3}"
    val tllRec_RecBase_2Base = "∃y1 ∃y2 ∃y3 ∃y4 ∃y5 ∃y6 ∃y7 ∃y8 ∃y9 . x1 ↦ (y1, y2, null) * y1 ↦ (y6, y4, null) * y2 ↦ (y9, y7, null) * tll(y6,x2,y5) * tll(y4,y5,y3) * tll(y9,y3,y8) * tll(y7,y8,x3) : {x1 ≉ x3, x2 ≉ x3, y1 ≉ y3, x2 ≉ y3, y2 ≉ x3, y3 ≉ x3}"
    val tllRec_2Rec_4Base = "∃y1 ∃y2 ∃y3 ∃y4 ∃y5 ∃y6 ∃y7 ∃y8 ∃y9 . x1 ↦ (y1, y2, null) * y1 ↦ (y6, y4, null) * y2 ↦ (y9, y7, null) * y6 ↦ (null, null, y5) * y4 ↦ (null, null, y3) * y9 ↦ (null, null, y8) * y7 ↦ (null, null, x3) : {x1 ≉ x3, x2 ≉ x3, y1 ≉ y3, x2 ≉ y3, y2 ≉ x3, y3 ≉ x3, y6 ≈ x2, y6 ≉ y5, y4 ≈ y5, y4 ≉ y3, y9 ≈ y3, y9 ≉ y8, y7 ≈ y8, y7 ≉ x3}"

    val testInputs : Seq[(String,Seq[String],String)] = Seq(
      ("sll(x1,x2)", Seq("emp : {x1 = x2}"), "emp : {x1 = x2}"),
      ("sll(x1,x2)", Seq("x1 -> y1 * sll(y1, x2)"), "x1 -> y1 * sll(y1, x2)"),
      ("sll(x2,x1)", Seq("x1 -> y1 * sll(y1, x2)"), "x2 -> y1 * sll(y1, x1)"),
      ("sll(x1,y1)", Seq("x1 -> y1 * sll(y1, x2)"), "x1 -> y2 * sll(y2, y1)"),
      ("sll(y1,y2)", Seq("x1 -> y1 * sll(y1, x2)"), "y1 -> y3 * sll(y3, y2)"),
      ("x1 -> (y1, y2) * tree(y1) * tree(y2)", Seq("x1 -> (null,null)", "x1 -> (null,null)"), "x1 -> (y1, y2) * y1 -> (null,null) * y2 -> (null,null)"),
      // TLL Cases
      (tllCall, Seq(tllBase), tllBase),
      (tllCall, Seq(tllRec), tllRec),
      (tllRec, Seq(tllBase, tllBase), tllRec_2Base),
      (tllRec, Seq(tllRec, tllBase), tllRec_RecBase),
      (tllRec, Seq(tllBase, tllRec), tllRec_BaseRec),
      (tllRec, Seq(tllRec, tllRec), tllRec_2Rec),
      (tllRec_2Rec, Seq(tllBase, tllBase, tllBase, tllBase), tllRec_2Rec_4Base)
    )

    for {
      (input,replacements,output) <- testInputs
    } {
      val unfoldBy : Seq[SymbolicHeap] = replacements map (_.parse)
      info("Testing equality " + input + unfoldBy.mkString("[",", ","]") + " == " + output)
      assert(input.parse.replaceCalls(unfoldBy) == output.parse)
    }

  }

}
