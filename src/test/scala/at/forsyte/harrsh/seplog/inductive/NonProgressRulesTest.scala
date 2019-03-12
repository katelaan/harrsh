package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.{ExampleSids, TestValues}
import at.forsyte.harrsh.test.HarrshTableTest

class NonProgressRulesTest extends HarrshTableTest with TestValues {

  lazy val ThreeCallsInNoProgress: RichSid = SidFactory.makeRootedSid("sll",
    "Singly-linked list",
    Map("sll" -> x1),
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(x1 =:= x2)),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y","z"), SymbolicHeap(P("sll")(x1, y1), P("sll")(y1, y2), P("sll")(y2, x2)))
  )

  lazy val EmptyPredInNoProgress: RichSid = SidFactory.makeRootedSid("odd",
    "Lists",
    Map("odd" -> x1, "even" -> x1, "sll" -> x1),
    ("sll", Seq("y"), SymbolicHeap(P("odd")(x1, y1), P("even")(y1, x2))),
    ("sll", Seq.empty, SymbolicHeap(P("even")(x1, x2))),
    ("odd", Seq("n"), SymbolicHeap(x1 -> y1, P("even")(y1, x2))),
    ("even", Seq("n"), SymbolicHeap(x1 -> y1, P("odd")(y1, x2))),
    ("even", Seq.empty, SymbolicHeap(x1 =:= x2))
  )

  val inputs = Table(
    ("sid", "no progress heaps"),
    (ExampleSids.NoProgressSll, Set(("sll", SymbolicHeap(P("sll")(x1, y1), P("sll")(y1, x2))))),
    (ThreeCallsInNoProgress, Set(
      ("sll", SymbolicHeap(P("sll")(x1, y1), P("sll")(y1, y2), P("sll")(y2, x2))),
      ("sll", SymbolicHeap(P("sll")(x1, y1), P("sll")(y1, y2), y2 =:= x2)),
      ("sll", SymbolicHeap(P("sll")(x1, y1), y1 =:= y2, P("sll")(y2, x2))),
      ("sll", SymbolicHeap(x1 =:= y1, P("sll")(y1, y2), P("sll")(y2, x2)))
    )),
    (EmptyPredInNoProgress, Set(
      ("sll", SymbolicHeap(P("odd")(x1, y1), P("even")(y1, x2))),
      ("sll", SymbolicHeap(P("odd")(x1, y1), y1 =:= x2)),
      ("sll", SymbolicHeap(P("even")(x1, x2)))
    ))
  )

  property("Computation of no-progress rules") {

    forAll(inputs) {
      (sid, expectation) =>
        Given(sid.toString)
        Then("The no-progress rules for merging should be " + expectation)
        val result = sid.empClosedNonProgressRules.map(pair => (pair._1.head, pair._2.body))
        result shouldEqual expectation
    }

  }

}
