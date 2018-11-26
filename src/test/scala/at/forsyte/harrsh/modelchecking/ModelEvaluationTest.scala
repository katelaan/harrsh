package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.Implicits
import at.forsyte.harrsh.test.HarrshTableTest

class ModelEvaluationTest extends HarrshTableTest with Implicits {

  val testCases = Table(
    ("symbolic heaps", "models", "expectation"),
    ("x1 -> (y1, null)".parse, Model.fromRSH("x1 -> (y1, null)".parse).get, true),
    ("x1 -> (y1, null)".parse, Model.fromRSH("x1 -> (y1, null) * y1 -> (null, x2)".parse).get, true),
    ("x1 -> (x2, null) : {x2 = x3}".parse, Model.fromRSH("x1 -> (x2, null) * x2 -> (null, x3)".parse).get, false),
    ("x1 -> (y1, null) : {y1 = x2}".parse, Model.fromRSH("x1 -> (y1, null) * y1 -> (null, x2)".parse).get, false),
    ("x1 -> (y1, null) : {y1 != x2, x1 != y1, x1 != null}".parse, Model.fromRSH("x1 -> (y1, null) * y1 -> (null, x2)".parse).get, true),
    ("x1 -> (y1, y2) : {y1 = null, y2 = x2}".parse, Model.fromRSH("x1 -> (null, x2)".parse).get, true),
    ("x1 -> (y1, y2) * y1 -> x2".parse, Model.fromRSH("x1 -> (null, x2)".parse).get, false),
    ("x1 -> x2".parse, Model.fromRSH("x1 -> (null, x2)".parse).get, false),
    ("x1 -> y1 * y1 -> y2 * y2 -> y3 : {y3 = x1}".parse, Model.fromRSH("x1 -> x2 * x2 -> x3 * x3 -> x1".parse).get, true)
  )

  property("Correctness of model evaluation") {
    forAll(testCases) {
      (rsh, model, expectation) =>
        model.evaluateReducedSymbolicHeap(rsh) shouldEqual expectation
    }
  }

}
