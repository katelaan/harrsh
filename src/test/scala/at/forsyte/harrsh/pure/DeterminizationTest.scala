package at.forsyte.harrsh.pure

import at.forsyte.harrsh.test.HarrshTableTest
import at.forsyte.harrsh.{Implicits, TestValues}
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 5/18/17.
  */
class DeterminizationTest extends HarrshTableTest with Implicits with TestValues {

  // Note: Write variables in increasing numeric value in the expectations
  val testValsForUndeterminedRelationships = Table(
    ("heap", "undetermined relationships"),
    ("x1 -> x2", Set((x1,x2), (nil,x2))),
    ("x1 -> null", Set()),
    ("x1 -> y1 * y1 -> null", Set()),
    ("x1 -> y1 * y1 -> x2", Set((x1,x2), (y1, x2), (nil,x2))),
    ("x1 -> x2 * x3 -> x4", Set((x1,x2), (nil,x2), (x3,x4), (nil,x4), (x2, x3), (x2, x4), (x1, x4))),
    ("emp : {x1 != x2, x2 = x3}", Set((nil,x1), (nil,x2)))
  )

  property("Correctness of computation of undetermined relationships") {

    forAll(testValsForUndeterminedRelationships) {
      (heap, expectedRels) =>
        info("Checking " + heap + " against expectation of undetermined relationships " + expectedRels)
        Determinization.undeterminedRelationships(heap.parse).toSet shouldEqual expectedRels
    }

  }

  val testValsForDeterminization = Table(
    ("heap", "expected determinizations"),
    ("x1 -> null", Set("x1 -> null")),
    ("x1 -> x2", Set("x1 ↦ x2 : {x1 ≈ x2, null ≈ x2}", "x1 ↦ x2 : {x1 ≉ x2, null ≈ x2}", "x1 ↦ x2 : {x1 ≈ x2, null ≉ x2}", "x1 ↦ x2 : {x1 ≉ x2, null ≉ x2}"))
  )

  property("Correctness of determinization") {

    forAll(testValsForDeterminization) {
      (heap, expectedDeterminizations) =>
        info("Checking " + heap + " against " + expectedDeterminizations.size + " expected determinization choices ")
        val determinization : Set[SymbolicHeap] = Determinization.rshDeterminizations(heap.parse).toSet
        val expectation : Set[SymbolicHeap] = expectedDeterminizations.map(_.parse)
        println("Diff 1 " + (determinization -- expectation))
        println("Diff 2 " + (expectation -- determinization))
        determinization shouldEqual expectation
    }

  }

}
