package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.Implicits
import at.forsyte.harrsh.parsers.SIDParsers.CyclistSIDParser
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 4/7/17.
  */
class CombinedParserTest extends HarrshTableTest with Implicits {

  val Success = true
  val Failure = false

  val inputs = Table(
    ("input", "expected result"),
    ("emp", Failure), // The parse function returns emp in case of failure, so this looks like a failure. Tested anyway to make sure there's no exception in the parse process.
    ("x -> y", Failure),
    ("x1 -> y", Success),
    ("x1 -> y1", Success),
    ("x1 -> y2", Success),
    ("x1 -> z", Success),
    ("{x1 = y1}", Failure),
    ("emp : {x1 = y1}", Success),
    ("emp : {x1 = y1,x2 = y3}", Success),
    ("emp * emp * emp * x1 -> x2 * emp * emp", Success),
    ("emp * emp * emp * x1 -> x2 * emp * emp : {x1 = y1,x2 = y3}", Success),
    ("emp * P(x1,x2,x3) * emp * x1 -> x2 * Q(a,b,c) * emp", Success),
    ("emp * P(x1,x2,x3) * emp * x1 -> x2 * Q(a,b,c) * emp : { a = b, c != d, e != f}", Success),
//    // Cyclist format
//    ("x1 = y1 * x1 -> y1", Success),
//    ("x1 = y1 * x1 -> y1", Success),
//    ("x1 != y1 * x1 -> y1", Success),
//    ("x1 = y1 * x1 -> y1 * x2 = z", Success),
//    ("emp * P(x1,y1) * y2 = y3 * * x1 -> y1 * P(x1, y1) * R(y1)", Failure),
    // Unicode parsing
    ("x1 ↦ y1 : {x1 = y1, x2 = y2}", Failure), // Mixed unicode / ascii
    (". x1 ↦ y1 : {x1 ≈ y1, x2 ≈ y2}", Success),
    ("x1 ↦ y1 : {x1 ≈ y1, x2 ≈ y2}", Success),
    ("∃y1 . x1 ↦ y1 : {x1 ≈ y1, x2 ≈ y2}", Success),
    ("∃y1 ∃y2 . x1 ↦ y1 : {x1 ≈ y1, x2 ≈ y2}", Success),
    ("∃y1 ∃y2 . x1 ↦ y1 * x2 ↦ y1 : {x1 ≈ y1, x2 ≈ y2}", Success),
    ("∃y1 ∃y2 . x1 ↦ y1 * P(x1,x2,y1) * x2 ↦ y1 : {x1 ≈ y1, x2 ≈ y2}", Success)
  )

  private def isSuccess(sh : SymbolicHeap) = sh != SymbolicHeap.empty

  property ("Correctness of combined parser") {
    forAll(inputs) {
      (input, expectedResult) =>
        val parseResult = input.parse

        info(""+parseResult)

        if (expectedResult) {
          isSuccess(parseResult) should be (expectedResult)
        }
    }
  }

}
