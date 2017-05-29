package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.test.HarrshTest
import at.forsyte.harrsh.Implicits._
import at.forsyte.harrsh.seplog.{PtrExpr, Var}
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 5/29/17.
  */
class ClosePartitionsUnderSymmetriesTest extends HarrshTest {

  behavior of "Closure of partititions under symmetries"

  it should "produce reversible permutations" in {

    val testheap = "x1 -> null * x2 -> (null, null) * x3 -> (null, null, null)".parse
    val call = PredCall("I", (1 to 3) map (i => PtrExpr(Var(i))))

    for {
      perm <- Combinators.permutations(Var.mkAllVars(1 to 3))
    } {
      val renamedHeap = SymbolicHeap.renameFVs(testheap, perm)
      info("Reversing renaming of " + testheap + " to " + renamedHeap)
      val newCall = ClosePartitionsUnderSymmetries.predCallReordering(call, perm)
      val context = SymbolicHeap(Seq.empty, Seq(newCall))
      val reversedHeap = context.replaceCall(newCall, renamedHeap)
      info("Result of reversal: " + reversedHeap)
      assert(testheap == reversedHeap)
    }

  }

}
