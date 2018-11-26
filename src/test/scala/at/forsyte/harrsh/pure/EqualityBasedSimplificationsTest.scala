package at.forsyte.harrsh.pure

import at.forsyte.harrsh.Implicits
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 4/11/17.
  */
class EqualityBasedSimplificationsTest extends HarrshTableTest with Implicits {

  val testVals = Table(
    ("input", "simplified output"),
    ("x1 = y1", "emp"),
    ("x1 = y1 * y1 = y2 * y2 = y3", "emp"),
    ("x1 -> (y1,y2,y3) : {y1 = x2, y3 = x3}", "x1 -> (x2,y1,x3)"),
    ("x1 -> (y1,y2,y3) : {y1 = x1, x1 != y2, y3 = x1}", "x1 -> (x1,y1,x1): {x1 != y1}"),
    ("x1 -> (y1,y2,y3) : {y1 = x2, y3 = x3, x2 != x3, y2 != x2}", "x1 -> (x2,y1,x3) : { x2 != x3, y1 != x2 }"),
    ("x1 -> (y1,y2,y3) * P(y3,y2,y1) : {y1 = x2, y3 = x3}", "x1 -> (x2,y1,x3) * P(x3,y1,x2)"),
    ("∃y1 ∃y2 ∃y3 ∃y4 ∃y5 ∃y6 ∃y7 ∃y8 ∃y9 . x1 ↦ (y1, y2, null) * y1 ↦ (y6, y4, null) * y2 ↦ (y9, y7, null) * y6 ↦ (null, null, y5) * y4 ↦ (null, null, y3) * y9 ↦ (null, null, y8) * y7 ↦ (null, null, x3) : {x1 ≉ x3, x2 ≉ x3, y1 ≉ y3, x2 ≉ y3, y2 ≉ x3, y3 ≉ x3, y6 ≈ x2, y6 ≉ y5, y4 ≈ y5, y4 ≉ y3, y9 ≈ y3, y9 ≉ y8, y7 ≈ y8, y7 ≉ x3}", "∃y1 ∃y2 ∃y3 ∃y4 ∃y5 . x1 ↦ (y1, y2, null) * y1 ↦ (x2, y4, null) * y2 ↦ (y3, y5, null) * x2 ↦ (null, null, y4) * y4 ↦ (null, null, y3) * y3 ↦ (null, null, y5) * y5 ↦ (null, null, x3) : {x1 ≉ x3, x2 ≉ x3, y1 ≉ y3, x2 ≉ y3, y2 ≉ x3, y3 ≉ x3, x2 ≉ y4, y4 ≉ y3, y3 ≉ y5, y5 ≉ x3}")
  )

  property("Transitive simplification of symbolic heaps") {

    forAll(testVals){
      (input, output) =>
        info("Simplification of " + input + " should be " + output)
        input.parse.simplify shouldEqual output.parse
    }

  }

}
