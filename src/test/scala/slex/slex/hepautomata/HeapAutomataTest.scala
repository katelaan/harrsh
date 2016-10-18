package slex.slex.hepautomata

import slex.heapautomata.{TrackingAutomata, _}
import slex.slex.SlexTableTest

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends SlexTableTest {

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

  property("On-the-fly emptiness checking") {

    forAll(emptinessChecks) {
      (automaton, sid, result) =>

        Given(sid + "\n and the automaton '" + automaton.description)

        Then("The emptiness check should return " + result)

        println("#"*80)
        println("Testing emptiness for refinement of " + sid + "\n with the automaton '" + automaton.description + "'; expected result: " + result)
        RefinementAlgorithms.onTheFlyEmptinessCheck(sid, automaton) should be(result)
        println()
    }

  }

}
