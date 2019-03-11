package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.{ExampleSids, TestValues}
import at.forsyte.harrsh.test.HarrshTableTest

class EmptyPredicatesTest extends HarrshTableTest with TestValues {

  private val TwoStepsEmpty = SidFactory.makeSid("a", "test",
    ("a", Seq.empty, SymbolicHeap(P("b")(x1, x2))),
    ("b", Seq.empty, SymbolicHeap(x2 =/= x1))
  )

  private val TwoStepsBranching = SidFactory.makeSid("a", "test",
    ("a", Seq("y"), SymbolicHeap(P("b")(x1, y1), P("b")(y1, x2))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2))
  )

  private val InterlockedBranching = SidFactory.makeSid("a", "test",
    ("a", Seq("y"), SymbolicHeap(P("c")(x1, y1), P("b")(y1, x2))),
    ("a", Seq.empty, SymbolicHeap(P("c")(x1, x2))),
    ("b", Seq.empty, SymbolicHeap(x1 =/= x2)),
    ("c", Seq.empty, SymbolicHeap(x1 =:= x2))
  )

  private val InterlockedBranching2 = SidFactory.makeSid("a", "test",
    ("a", Seq("y"), SymbolicHeap(P("a")(x1, y1), P("b")(y1, x2))),
    ("b", Seq.empty, SymbolicHeap(x1 =/= x2)),
    ("a", Seq.empty, SymbolicHeap(x1 =:= x2))
  )

  private val MultiOptions = SidFactory.makeSid("a", "test",
    ("a", Seq.empty, SymbolicHeap(P("b")(x1, x2), P("b")(x2, x3))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("b", Seq.empty, SymbolicHeap(x1 =/= x2))
  )

  private val x1_eq_x2 = Set(Set(x1 =:= x2))
  private val x1_neq_x2 = Set(Set(x1 =/= x2))
  private val x2_neq_x1 = Set(Set(x2 =/= x1))
  private val x1_eq_or_neq_x2 = Set(Set(x1 =:= x2), Set(x1 =/= x2))

  val inputs = Table(
    ("sid", "result"),
    (ExampleSids.Sll, Map("sll" -> x1_eq_x2)),
    (ExampleSids.NoProgressSll, Map("sll" -> x1_eq_x2)),
    // The recursive step orders the atoms, which is why we get a different order for a than for b.
    (TwoStepsEmpty, Map("a" -> x1_neq_x2, "b" -> x2_neq_x1)),
    (TwoStepsBranching, Map("a" -> x1_eq_x2, "b" -> x1_eq_x2)),
    (InterlockedBranching, Map("a" -> x1_eq_or_neq_x2, "b" -> x1_neq_x2, "c" -> x1_eq_x2)),
    // Note: The empty set is possible here; it will be discovered in the third iteration,
    // because x_1 != y_1 and y_1 != x_2 does not imply x_1 != x_2
    (InterlockedBranching2, Map("a" -> Set(Set(x1 =:= x2), Set(x1 =/= x2), Set.empty), "b" -> x1_neq_x2)),
    (MultiOptions, Map("a" -> Set(
      Set(x1 =:= x2, x2 =:= x3, x1 =:= x3),
      Set(x1 =:= x2, x2 =/= x3, x1 =/= x3),
      Set(x1 =/= x2, x2 =:= x3, x1 =/= x3),
      Set(x1 =/= x2, x2 =/= x3)), "b" -> x1_eq_or_neq_x2))
  )

  property("Computation of empty predicates") {

    forAll(inputs) {
      (sid, result) =>
        Given(sid.toString)
        Then("The pure constraints for empty unfoldings of the predicate should be " + result)
        EmptyPredicates(sid).underlying shouldEqual result
    }

  }
}
