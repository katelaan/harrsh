package at.forsyte.harrsh.pure

import at.forsyte.harrsh.Implicits
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 5/16/17.
  */
class ReducedHeapEquivalenceTest extends HarrshTableTest with Implicits {

  val AreEquivalent = true
  val NotEquivalent = !AreEquivalent

  val inputs = Table(
    ("fst", "snd", "expected"),
    // Equal heaps should be equivalent
    ("x1 -> x2".parse, "x1 -> x2".parse, AreEquivalent),
    ("emp".parse, "emp".parse, AreEquivalent),
    ("x1 -> x2 : {x1 = x2}".parse, "x1 -> x2 : {x1 = x2}".parse, AreEquivalent),
    ("x1 -> y1 * y1 -> x2 : {x1 = x2}".parse, "x1 -> y1 * y1 -> x2 : {x1 = x2}".parse, AreEquivalent),
    // Reordering atoms and equalities shouldn't matter
    ("x1 -> x2 : {x1 = x2}".parse, "x1 -> x2 : {x2 = x1}".parse, AreEquivalent),
    ("y1 -> x2 * x1 -> y1 : {x1 = x2}".parse, "x1 -> y1 * y1 -> x2 : {x2 = x1}".parse, AreEquivalent),

    // Changing free variables does not preserve equivalence...
    ("x1 -> x2".parse, "x1 -> x3".parse, NotEquivalent),
    ("x1 -> x2 : {x1 = x2}".parse, "x3 -> x2 : {x3 = x2}".parse, NotEquivalent),
    ("x3 -> y1 * y1 -> x2 : {x2 = x3}".parse, "x1 -> y1 * y1 -> x2 : {x1 = x2}".parse, NotEquivalent),

    // ...whereas just renaming/reordering bound variables does
    ("x1 -> y1 * y1 -> y2 * y2 -> x2".parse, "x1 -> y2 * y2 -> y1 * y1 -> x2".parse, AreEquivalent),
    ("x1 -> y1 * y1 -> y2 * y2 -> y3 * y3 -> x1".parse, "x1 -> y3 * y3 -> y2 * y2 -> y1 * y1 -> x1".parse, AreEquivalent),

    // Adding redundant (in)-equalities shouldn't matter...
    ("x1 -> y1 * y1 -> x2 : {x1 = x2}".parse, "x1 -> y1 * y1 -> x2 : {x1 = x2, x2 = x1}".parse, AreEquivalent),
    ("x1 -> y1 * y1 -> x2 : {x1 = x2}".parse, "x1 -> y1 * y1 -> x2 : {x1 = x2, x2 = x1, y1 != x1, x1 != y1}".parse, AreEquivalent),

    // ...whereas adding non-redundant (in)equalities should not preserve equivalence
    ("x1 -> x2".parse, "x1 -> x2 : {x1 = x2}".parse, NotEquivalent),
    ("x1 -> x2".parse, "x1 -> x2 : {x1 != x2}".parse, NotEquivalent),
    ("x1 -> x2".parse, "x1 -> x2 : {x1 = x3}".parse, NotEquivalent),
    ("x1 -> x2".parse, "x1 -> x2 : {x1 != x3}".parse, NotEquivalent),
    ("x1 -> y1 * y1 -> x2".parse, "x1 -> y1 * y1 -> x2 : {y1 = x2}".parse, NotEquivalent),

    // Unsatisfiable heaps should be equivalent,
    ("emp : {x1 != x1}".parse, "emp : {x1 != x1}".parse, AreEquivalent),
    ("emp : {x2 != x2}".parse, "emp : {x1 != x1}".parse, AreEquivalent),

    // If only some determinizations imply each other, the overall equivalence should not hold
    ("∃y1 . y1 ↦ (null, null) * x1 ↦ (y1, x2) : {y1 ≉ x2}".parse, "∃y1 . y1 ↦ (null, null) * x1 ↦ (x2, y1) : {y1 ≉ x2}".parse, NotEquivalent),
    ("∃y1 . y1 ↦ (null, null) * x1 ↦ (y1, x2) : {y1 ≈ x2}".parse, "∃y1 . y1 ↦ (null, null) * x1 ↦ (x2, y1) : {y1 ≈ x2}".parse, AreEquivalent),
    ("∃y1 . y1 ↦ (null, null) * x1 ↦ (y1, x2)".parse, "∃y1 . y1 ↦ (null, null) * x1 ↦ (x2, y1)".parse, NotEquivalent)
    // TODO For reduced heap equivalence, add a few more test cases where there exist both determinizations that are models and that are not models (like the previous one)
  )

  property("Correctness of reduced heap equivalence") {

    forAll(inputs) {
      (fst: SymbolicHeap, snd: SymbolicHeap, expectedResult: Boolean) =>
        Given(fst + ", " + snd)
        Then("Equivalence should " + (if (expectedResult) "HOLD" else "NOT HOLD"))
        ReducedHeapEquivalence(fst, snd) shouldBe expectedResult
    }

  }

  /*
   * Test single input with verbose output for local debugging
   */
//  def check(fst: SymbolicHeap, snd: SymbolicHeap, expectedResult: Boolean) = {
//    println(fst + " " + snd)
//    println("Equivalence should " + (if (expectedResult) "HOLD" else "NOT HOLD"))
//    val res = ReducedHeapEquivalence(fst, snd, reportProgress = true)
//    println(res)
//    res shouldBe expectedResult
//  }
  //check("∃y1 . y1 ↦ (null, null) * x1 ↦ (y1, x2)".parse, "∃y1 . y1 ↦ (null, null) * x1 ↦ (x2, y1)".parse, NotEquivalent)

}
