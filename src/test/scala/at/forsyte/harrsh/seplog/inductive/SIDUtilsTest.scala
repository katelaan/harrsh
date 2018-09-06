package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.test.HarrshTableTest
import at.forsyte.harrsh.Implicits._

class SIDUtilsTest extends HarrshTableTest with TestValues {

  // TODO: Include test cases where normalization fails

  val inputs = Table(
    ("input", "output preds"),
    (SymbolicHeap.empty, Seq(mkPred("P1", SymbolicHeap.empty))),
    (SymbolicHeap(x1 -> (x2, x3)), Seq(mkPred("P1", "x1 ↦ (x2, x3)".parse))),
    (SymbolicHeap(x1 -> (y1, x3), P("Q")(y1, x2)), Seq(mkPred("P1", "∃_1 . x1 ↦ (_1, x3) * Q(_1,x2)".parse))),
    (SymbolicHeap(P("Q")(x2, x1)), Seq(mkPred("P1", "Q(x2,x1)".parse))),
    (SymbolicHeap(P("Q")(x1, x2), P("R")(x3, x4)), Seq(mkPred("P1", "Q(x1,x2) * R(x3,x4)".parse))),
    (SymbolicHeap(P("Q")(x1, y1), P("R")(y1, y2)), Seq(mkPred("P1", "Q(x1,_1) * R(_1,_2)".parse))),
    (SymbolicHeap(x1 -> (x2, x3), x3 -> x4),
      Seq(
        mkPred("P1", "x1 ↦ (x2, x3) * P2(x3,x4)".parse),
        mkPred("P2", "x1 ↦ x2".parse))),
    // y1 is shared between the two pointers, so it's quantified in P1 and passed as additional parameter to P2
    (SymbolicHeap(x1 -> (x2, y1), y1 -> x3),
      Seq(
        mkPred("P1", "x1 ↦ (x2, _1) * P2(x3,_1)".parse),
        // Note that the pointer is flipped here, because P2's arg list receives the bound variable after the free variable
        mkPred("P2", "x2 ↦ x1".parse))),
    // y1 is used (only) in P1, so it's quantified there
    // y2 is shared between the two pointers, so it's quantified in P1 and passed as parameter to P2
    // y3 is not shared, so it's quantified in P2
    (SymbolicHeap(x1 -> (y1, y2), y2 -> y3),
      Seq(
        mkPred("P1", "x1 ↦ (_1, _2) * P2(_2)".parse),
        mkPred("P2", "x1 ↦ _1".parse))),
    // Pure atoms go into the top-level predicate P1...
    (SymbolicHeap(x1 -> (x2, x3), x3 -> x4, x1 =/= x2, x1 =:= x4),
      Seq(
        mkPred("P1", "x1 ↦ (x2, x3) * P2(x3,x4) : {x1 ≉ x2, x1 ≈ x4}".parse),
        mkPred("P2", "x1 ↦ x2".parse))),
    // ...also if they mention bound variables, which results in them being treated as shared.
    // For this reason, P2 now receives two parameters rather than one as in the case without pure constraints
    (SymbolicHeap(x1 -> (y1, y2), y2 -> y3, x1 =/= y1, x1 =:= y3),
      Seq(
        mkPred("P1", "x1 ↦ (_1, _2) * P2(_2, _3) : {x1 ≉ _1, x1 ≈ _3}".parse),
        mkPred("P2", "x1 ↦ x2".parse))),
    // Predicate calls are deferred into the "bottom-most" predicate
    // Note again the reordering/renaming of the vars in the pointer in P2, because free vars are passed to P2 before quantified vars
    (SymbolicHeap(x1 -> (y1, y2), y2 -> y3, x1 =/= y1, x1 =:= y3, P("q")(y3), P("r")(x2)),
      Seq(
        mkPred("P1", "x1 ↦ (_1, _2) * P2(x2, _2, _3) : {x1 ≉ _1, x1 ≈ _3}".parse),
        mkPred("P2", "x2 ↦ x3 * q(x3) * r(x1)".parse)))
  )

  property("Correct progress normal form computation") {
    forAll(inputs) {
      (sh, expectedResult) =>
        val conversionResult = SIDUtils.shToProgressSid(sh, "P")

        info("" + conversionResult)

        conversionResult.preds should be(expectedResult)
    }
  }

  private def mkPred(predName: String, ruleSh: SymbolicHeap): Predicate = {
    Predicate(predName, Seq(SIDUtils.shToRuleBody(ruleSh)))
  }
}
