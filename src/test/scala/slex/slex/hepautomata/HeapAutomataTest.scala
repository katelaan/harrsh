package slex.slex.hepautomata

import slex.heapautomata.{TrackingAutomata, _}
import slex.slex.SlexTableTest

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends SlexTableTest {

  val inconsistent2 = new BaseTrackingAutomaton(2, (_,_,_) => false, "dummy").InconsistentState

  val emptinessChecks = Table(
    ("automaton", "sid", "result"),
    /*
     * Has pointer automaton
     */
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Sll, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Dll, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Tree, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Tll, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.EmptyLinearPermuter, true),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.NonEmptyLinearPermuter, false),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.NonEmptyBinaryPermuter, false),
    /*
     * Tracking automata
      */
    // Normal tests
    (TrackingAutomata.singleTargetStateTracking(3, Set(fv(1)), mkPure()), ExampleSIDs.Sll, false),
    (TrackingAutomata.singleTargetStateTracking(2, Set(fv(1)), mkPure((1, 2, false))), ExampleSIDs.Sll, true),
    (TrackingAutomata.singleTargetStateTracking(2, Set(fv(1)), mkPure()), ExampleSIDs.EmptyLinearPermuter, true),
    (TrackingAutomata.singleTargetStateTracking(2, Set(), mkPure((1, 2, true))), ExampleSIDs.EmptyLinearPermuter, false),
    (TrackingAutomata.singleTargetStateTracking(4, Set(fv(1),fv(4)), mkPure()), ExampleSIDs.Dll, true),
    (TrackingAutomata.singleTargetStateTracking(1, Set(fv(1)), mkPure()), ExampleSIDs.Tree, false),
    (TrackingAutomata.singleTargetStateTracking(3, Set(fv(1),fv(2)), mkPure((1,2,false))), ExampleSIDs.Tll, false),
    // Inconsistency checks for tracking
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter, false),
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter2, false),
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter3, false),
    /*
     * SAT automata
     */
    // - on SIDs that produce at least one satisfiable heap
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.Sll, false),
    (TrackingAutomata.satAutomaton(4), ExampleSIDs.Dll, false),
    (TrackingAutomata.satAutomaton(1), ExampleSIDs.Tree, false),
    (TrackingAutomata.satAutomaton(3), ExampleSIDs.Tll, false),
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.EmptyLinearPermuter, false),
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.NonEmptyLinearPermuter, false),
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter, false),
    /*
     * UNSAT automata
     */
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.UnsatSID, true),
    // - with consistent SIDs that do not produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.Sll, true),
    (TrackingAutomata.unsatAutomaton(4), ExampleSIDs.Dll, true),
    (TrackingAutomata.unsatAutomaton(1), ExampleSIDs.Tree, true),
    (TrackingAutomata.unsatAutomaton(3), ExampleSIDs.Tll, true),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.EmptyLinearPermuter, true),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyLinearPermuter, true),
    // - with inconsistent SIDs that do produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter, false),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter2, false),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter3, false),
    //- with unsatisfiable SIDs
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.UnsatSID, false),

    /*
     * Establishment automata
     */
    // - with established data structure SIDs
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.Sll, false),
    (TrackingAutomata.establishmentAutomaton(4), ExampleSIDs.Dll, false),
    (TrackingAutomata.establishmentAutomaton(1), ExampleSIDs.Tree, false),
    (TrackingAutomata.establishmentAutomaton(3), ExampleSIDs.Tll, false)
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
