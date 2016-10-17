package slex.slex.hepautomata

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import slex.SlexTest
import slex.heapautomata.TrackingAutomata
import slex.heapautomata._
import slex.heapautomata.utils.ClosureOfAtomSet
import slex.seplog.{nil, ptr, PtrEq, PureAtom, SymbolicHeap, call}

/**
  * Created by jens on 10/16/16.
  */
class TrackingAutomataTest extends SlexTest with TableDrivenPropertyChecks {

  private def mkPure(atoms : (Int, Int, Boolean)*) : Set[PureAtom] = Set() ++ (atoms.toSeq map {
    case (l,r,isEq) => orderedAtom(fv(l),fv(r),isEq)
  })

  private def fvAll(ints : Int*) : Set[FV] = Set() ++ ints map fv

  println("Checking congruence closure computation")

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
      val closure = new ClosureOfAtomSet(eqs)

      for (i <- 1 to 5) {
        println(fv(i) + (if (results(i-1)) " should be " else " should NOT be ") + "the minimal element in an equality class of " + eqs)
        closure.isMinimumInItsClass(fv(i)) should be(results(i - 1))
      }
  }

  println("Testing defined-ness of transitions")
  val track3 = TrackingAutomata(3, Set(fv(1)), mkPure((2, 3, true)))

  val transitions = Table(
    ("src", "trg", "sh", "result"),
    // Simple RSHs
    (Seq(), (fvAll(1), mkPure()), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), true),
    (Seq(), (fvAll(2), mkPure()), SymbolicHeap(Seq(ptr(fv(1), fv(2)))), false),
    (Seq(), (fvAll(1,2), mkPure((1,2,true))), SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(ptr(fv(1), fv(2)))), true),

    // RSHs with some propagation
    (Seq(), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(ptr(fv(1), nil), ptr(fv(2), nil))), true),

    // Non-reduced SHs without parameter renaming
    (Seq((fvAll(1), mkPure())), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(2), nil))), true),
    (Seq((fvAll(1), mkPure()), (fvAll(), mkPure((1, 2, true)))),
      (fvAll(1,2,3), mkPure((1,2,true), (1,3,false), (2,3,false))),
      SymbolicHeap(Seq(call("foo", fv(1)), call("bar", fv(1), fv(2)), ptr(fv(3), nil))),
      true),

    // Non-reducsed SHs with parameter renaming
    (Seq((fvAll(1), mkPure((1,2,false)))),
    (fvAll(1), mkPure()), // Note: We do not know that x_1 != x_2, but only x_1 != y && x_2 != y; the list may be cyclic
    SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")), // 2nd rule of SLL predicate
    true)
  )

  forAll(transitions) {
    (src : Seq[track3.State], trg: track3.State, sh : SymbolicHeap, result : Boolean) =>
      track3.isTransitionDefined(src, trg, sh) should be (result)
  }

  // RSHs
  // TODO Clean up with state constructors + table-driven testing
  // Trivial test cases
//  track3.isTransitionDefined(Seq(), (fvAll(1), mkPure()), SymbolicHeap(Seq(ptr(fv(1), fv(2))))) should be (true)
//  track3.isTransitionDefined(Seq(), (fvAll(2), mkPure()), SymbolicHeap(Seq(ptr(fv(1), fv(2))))) should be (false)
//  track3.isTransitionDefined(Seq(), (fvAll(1,2), mkPure((1,2,true))), SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(ptr(fv(1), fv(2))))) should be (true)

//  track3.isTransitionDefined(Seq(), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(ptr(fv(1), nil), ptr(fv(2), nil)))) should be (true)

  // Non-reduced SHs
//  track3.isTransitionDefined(Seq((fvAll(1), mkPure())), (fvAll(1,2), mkPure((1,2,false))), SymbolicHeap(Seq(call("dummy", fv(1)), ptr(fv(2), nil)))) should be (true)

//  track3.isTransitionDefined(
//    src = Seq((fvAll(1), mkPure()), (fvAll(), mkPure((1, 2, true)))),
//    trg = (fvAll(1,2,3), mkPure((1,2,true), (1,3,false), (2,3,false))),
//    lab = SymbolicHeap(Seq(call("foo", fv(1)), call("bar", fv(1), fv(2)), ptr(fv(3), nil)))
//  ) should be (true)

  println("Testing emptiness for the Tracking automaton")
//  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.Sll, track3) should be (true)
//  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.EmptyLinearPermuter, track3) should be (true)
//  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyLinearPermuter, track3) should be (true)
//  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyBinaryPermuter, track3) should be (true)

}
