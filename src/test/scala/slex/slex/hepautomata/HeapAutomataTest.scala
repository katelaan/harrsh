package slex.slex.hepautomata

import org.scalatest.prop.TableDrivenPropertyChecks
import slex.SlexTest
import slex.heapautomata.{TrackingAutomata, _}

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends SlexTest with TableDrivenPropertyChecks {

  println("Testing emptiness checking")

  val inconsistent2 = TrackingAutomata(2, Set(), Set()).InconsistentState

  val emptinessChecks = Table(
    ("automaton", "sid", "result"),
    // Has pointer automaton
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Sll, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Dll, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Tree, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Tll, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.EmptyLinearPermuter, true),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.NonEmptyLinearPermuter, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.NonEmptyBinaryPermuter, false),
    // Tracking automata, normal tests
    (TrackingAutomata(3, Set(fv(1)), mkPure()), ExampleSIDs.Sll, false),
    (TrackingAutomata(2, Set(fv(1)), mkPure((1, 2, false))), ExampleSIDs.Sll, true),
    (TrackingAutomata(2, Set(fv(1)), mkPure()), ExampleSIDs.EmptyLinearPermuter, true),
    (TrackingAutomata(2, Set(), mkPure((1, 2, true))), ExampleSIDs.EmptyLinearPermuter, false),
    (TrackingAutomata(4, Set(fv(1),fv(4)), mkPure()), ExampleSIDs.Dll, true),
    (TrackingAutomata(1, Set(fv(1)), mkPure()), ExampleSIDs.Tree, false),
    (TrackingAutomata(3, Set(fv(1),fv(2)), mkPure((1,2,false))), ExampleSIDs.Tll, false),
    // Inconsistency checks
    (TrackingAutomata(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter, false),
    (TrackingAutomata(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter2, false),
    (TrackingAutomata(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter3, false)
  )

  forAll(emptinessChecks) {
    (automaton, sid, result) =>
      println("#"*80)
      println("Testing emptiness for refinement of " + sid + "\n with the automaton '" + automaton.description + "'; expected result: " + result)
      RefinementAlgorithms.onTheFlyEmptinessCheck(sid, automaton) should be (result)
      println()
  }

  //  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.EmptyLinearPermuter, track3) should be (true)
  //  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyLinearPermuter, track3) should be (true)
  //  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyBinaryPermuter, track3) should be (true)




}
