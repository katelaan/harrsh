package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.test.HarrshTableTest

class ModelFromTuplesTest extends HarrshTableTest {

  val testCases = Table(
    ("model", "serialization"),
    (Model.fromTuples( (1, List(2,3), List(Var(1))) ),
      """Stack {
        |  x1 -> 1
        |}
        |Heap {
        |  1 -> 2, 3
        |}""".stripMargin),
    (Model.fromTuples( (1, List(2,3), List(Var(1))), (2, List(4,0,4), List()), (4, List(1,0,3), List(Var(2),Var(3))) ),
      """Stack {
        |  x1 -> 1
        |  x2 -> 4
        |  x3 -> 4
        |}
        |Heap {
        |  1 -> 2, 3
        |  2 -> 4, 0, 4
        |  4 -> 1, 0, 3
        |}""".stripMargin)
  )

  property("Model construction from tuples") {
    forAll(testCases) {
      (model, expectation) =>
        model.toString shouldEqual expectation
    }
  }
}
