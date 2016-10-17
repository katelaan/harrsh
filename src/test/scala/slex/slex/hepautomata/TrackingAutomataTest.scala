package slex.slex.hepautomata

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import slex.SlexTest
import slex.heapautomata.{ExampleSIDs, RefinementAlgorithms, ToyExampleAutomata, TrackingAutomata}
import slex.heapautomata._
import slex.seplog.{NullPtr, PointsTo, PredCall, PtrEq, PureAtom, SymbolicHeap}

/**
  * Created by jens on 10/16/16.
  */
class TrackingAutomataTest extends SlexTest with TableDrivenPropertyChecks {

  println("Checking congruence closure computation")

  private def mkPure(atoms : (Int, Int, Boolean)*) : Set[PureAtom] = Set() ++ (atoms.toSeq map {
    case (l,r,isEq) => orderedAtom(fv(l),fv(r),isEq)
  })

  private def fvAll(ints : Int*) : Set[FV] = Set() ++ ints map fv

  // Test closure computation
  val fveqs = Table(
    ("pure", "results"),
    (mkPure(), Seq(true,true,true,true,true)),
    (mkPure((1,2,true), (2,3,true)), Seq(true, false, false, true, true)),
    (mkPure((1,2,true), (2,3,false)), Seq(true, false, true, true, true)),
    (mkPure((1,2,true), (2,3,true), (4,5,true), (3,4,true)), Seq(true,false,false,false,false)),
    (mkPure((1,2,true), (2,3,false), (4,5,true), (3,4,true)), Seq(true,false,true,false,false))
  )

  forAll(fveqs) {
    (eqs : Set[PureAtom], results : Seq[Boolean]) =>
      val isRep = TrackingAutomata.computeKernelFromEqualities(eqs)

      for (i <- 1 to 5) {
        println("Representation of " + eqs + " applied to " + i + " should yield " + results(i-1))
        isRep(fv(i)) should be(results(i - 1))
      }
  }

  println("Testing defined-ness of transitions")
  val track3 = TrackingAutomata(3, Set(fv(1)), mkPure((2, 3, true)))

  import TrackingAutomata._

  // RSHs
  // TODO Clean up with state constructors + table-driven testing
  track3.isTransitionDefined(Seq(), (fvAll(1), mkPure()), SymbolicHeap(Seq(PointsTo(fv(1), fv(2))))) should be (true)
  track3.isTransitionDefined(Seq(), (fvAll(2), mkPure()), SymbolicHeap(Seq(PointsTo(fv(1), fv(2))))) should be (false)
  track3.isTransitionDefined(Seq(), (fvAll(1,2), mkPure((1,2,true))), SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(PointsTo(fv(1), fv(2))))) should be (true)
  track3.isTransitionDefined(Seq(), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(PointsTo(fv(1), NullPtr()), PointsTo(fv(2), NullPtr())))) should be (true)

  // Non-reduced SHs
  track3.isTransitionDefined(Seq((fvAll(1), mkPure())), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(PredCall("dummy", fv(1)), PointsTo(fv(2), NullPtr())))) should be (true)

  track3.isTransitionDefined(
    src = Seq((fvAll(1), mkPure()), (fvAll(), mkPure((1, 2, true)))),
    trg = (fvAll(1,2,3), mkPure((1,2,true), (1,3,false), (2,3,false))),
    lab = SymbolicHeap(Seq(PredCall("foo", fv(1)), PredCall("bar", fv(1), fv(2)), PointsTo(fv(3), NullPtr())))
  ) should be (true)

  println("Testing emptiness for the Tracking automaton")
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.Sll, track3) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.EmptyLinearPermuter, track3) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyLinearPermuter, track3) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyBinaryPermuter, track3) should be (true)

}