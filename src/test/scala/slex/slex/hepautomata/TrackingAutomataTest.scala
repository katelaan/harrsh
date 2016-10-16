package slex.slex.hepautomata

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import slex.SlexTest
import slex.heapautomata.{ExampleSIDs, RefinementAlgorithms, ToyExampleAutomata, TrackingAutomata}
import slex.heapautomata.TrackingAutomata.FVEquality
import slex.seplog.{NullPtr, PointsTo, PredCall, PtrEq, SymbolicHeap}

/**
  * Created by jens on 10/16/16.
  */
class TrackingAutomataTest extends SlexTest with TableDrivenPropertyChecks {

  println("Checking congruence closure computation")

  // Test closure computation
  val fveqs = Table(
    ("pure", "results"),
    (Set[FVEquality](), Seq(true,true,true,true,true)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,true)), Seq(true, false, false, true, true)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,false)), Seq(true, false, true, true, true)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,true), FVEquality(4,5,true), FVEquality(3,4,true)), Seq(true,false,false,false,false)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,false), FVEquality(4,5,true), FVEquality(3,4,true)), Seq(true,false,true,false,false))
  )

  forAll(fveqs) {
    (eqs : Set[FVEquality], results : Seq[Boolean]) =>
      val isRep = TrackingAutomata.computeRepresentationsInClosure(eqs)

      for (i <- 1 to 5) {
        println("Representation of " + eqs + " applied to " + i + " should yield " + results(i-1))
        isRep(i) should be(results(i - 1))
      }
  }

  println("Testing defined-ness of transitions")
  val track3 = TrackingAutomata(3, Set(1), Set(FVEquality(2, 3, true)))

  import TrackingAutomata._

  // RSHs
  // TODO Clean up with state constructors + table-driven testing
  track3.isTransitionDefined(Seq(), (Set(1), Set()), SymbolicHeap(Seq(PointsTo(fv(1), fv(2))))) should be (true)
  track3.isTransitionDefined(Seq(), (Set(2), Set()), SymbolicHeap(Seq(PointsTo(fv(1), fv(2))))) should be (false)
  track3.isTransitionDefined(Seq(), (Set(1,2), Set(FVEquality(1,2,true))), SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(PointsTo(fv(1), fv(2))))) should be (true)
  track3.isTransitionDefined(Seq(), (Set(1,2), Set(FVEquality(1,2,false))), SymbolicHeap(Seq(PointsTo(fv(1), NullPtr()), PointsTo(fv(2), NullPtr())))) should be (true)

  // Non-reduced SHs
  track3.isTransitionDefined(Seq((Set(1), Set[FVEquality]())), (Set(1,2), Set(FVEquality(1,2,false))), SymbolicHeap(Seq(PredCall("dummy", fv(1)), PointsTo(fv(2), NullPtr())))) should be (true)

  track3.isTransitionDefined(
    src = Seq((Set(1), Set[FVEquality]()), (Set[FV](), Set(FVEquality(1, 2, true)))),
    trg = (Set(1,2,3), Set(FVEquality(1,2,true), FVEquality(1,3,false), FVEquality(2,3,false))),
    lab = SymbolicHeap(Seq(PredCall("foo", fv(1)), PredCall("bar", fv(1), fv(2)), PointsTo(fv(3), NullPtr())))
  ) should be (true)

  println("Testing emptiness for the Tracking automaton")
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.Sll, track3) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.EmptyLinearPermuter, track3) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyLinearPermuter, track3) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyBinaryPermuter, track3) should be (true)

}
