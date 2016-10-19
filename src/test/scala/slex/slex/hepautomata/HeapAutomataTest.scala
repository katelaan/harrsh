package slex.slex.hepautomata

import slex.heapautomata.{TrackingAutomata, _}
import slex.slex.SlexTableTest

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends SlexTableTest {

  val inconsistent2 = new BaseTrackingAutomaton(2, (_,_,_) => false, "dummy").InconsistentState

  val Empty = true
  val NonEmpty = false

  val emptinessChecks = Table(
    ("automaton", "sid", "result"),

    /*
     * Has pointer automaton
     */
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Sll, NonEmpty),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Dll, NonEmpty),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Tree, NonEmpty),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.Tll, NonEmpty),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.EmptyLinearPermuter, Empty),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.NonEmptyLinearPermuter, NonEmpty),
    (ToyExampleAutomata.HasPointerAutomaton, ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),

    /*
     * Tracking automata
      */
    // Normal tests
    (TrackingAutomata.singleTargetStateTracking(3, Set(fv(1)), mkPure()), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(2, Set(fv(1)), mkPure((1, 2, NonEmpty))), ExampleSIDs.Sll, Empty),
    (TrackingAutomata.singleTargetStateTracking(2, Set(fv(1)), mkPure()), ExampleSIDs.EmptyLinearPermuter, Empty),
    (TrackingAutomata.singleTargetStateTracking(2, Set(), mkPure((1, 2, Empty))), ExampleSIDs.EmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(4, Set(fv(1),fv(4)), mkPure()), ExampleSIDs.Dll, Empty),
    (TrackingAutomata.singleTargetStateTracking(1, Set(fv(1)), mkPure()), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(3, Set(fv(1),fv(2)), mkPure((1,2,NonEmpty))), ExampleSIDs.Tll, NonEmpty),
    // Inconsistency checks for tracking
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter2, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2._1, inconsistent2._2), ExampleSIDs.NonEmptyBinaryPermuter3, NonEmpty),

    /*
     * SAT automata
     */
    // - on SIDs that produce at least one satisfiable heap
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.satAutomaton(4), ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.satAutomaton(1), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.satAutomaton(3), ExampleSIDs.Tll, NonEmpty),
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.EmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.NonEmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),

    /*
     * UNSAT automata
     */
    (TrackingAutomata.satAutomaton(2), ExampleSIDs.UnsatSID, Empty),
    // - with consistent SIDs that do not produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.Sll, Empty),
    (TrackingAutomata.unsatAutomaton(4), ExampleSIDs.Dll, Empty),
    (TrackingAutomata.unsatAutomaton(1), ExampleSIDs.Tree, Empty),
    (TrackingAutomata.unsatAutomaton(3), ExampleSIDs.Tll, Empty),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.EmptyLinearPermuter, Empty),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyLinearPermuter, Empty),
    // - with inconsistent SIDs that do produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter2, NonEmpty),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.NonEmptyBinaryPermuter3, NonEmpty),
    //- with unsatisfiable SIDs
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.UnsatSID, NonEmpty),
    (TrackingAutomata.unsatAutomaton(2), ExampleSIDs.UnsatSID2, NonEmpty),

    /*
     * Establishment automata
     */
    // - with established data structure SIDs
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.establishmentAutomaton(4), ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.establishmentAutomaton(1), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.establishmentAutomaton(3), ExampleSIDs.Tll, NonEmpty),
    // - with unsat SIDs (Inconsistent heaps are established in our current interpretation)
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.UnsatSID, NonEmpty),
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.UnsatSID2, NonEmpty),
    // - with partially-established SIDs
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID, NonEmpty),
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID2, NonEmpty),
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID3, NonEmpty),
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID4, NonEmpty),
    // - non-established SIDs
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.NonEstablishedSID, Empty),
    (TrackingAutomata.establishmentAutomaton(2), ExampleSIDs.NonEstablishedSID2, Empty),

    /*
     * Non-Establishment automata
     */
    // - with established data structure SIDs
    (TrackingAutomata.nonEstablishmentAutomaton(2), ExampleSIDs.Sll, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton(4), ExampleSIDs.Dll, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton(1), ExampleSIDs.Tree, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton(3), ExampleSIDs.Tll, Empty),
    // - with partially-established SIDs
    (TrackingAutomata.nonEstablishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID2, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID3, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID4, NonEmpty)

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

  // Note: To try a single test case, comment out the following

//  val extraAUT = TrackingAutomata.establishmentAutomaton(2)
//  val extraSID = ExampleSIDs.NonEstablishedSID2
//  val extraRes = Empty
//
//  println("Testing emptiness for refinement of " + extraSID + "\n with the automaton '" + extraAUT.description + "'; expected result: " + extraRes)
//  RefinementAlgorithms.onTheFlyEmptinessCheck(extraSID, extraAUT) should be(extraRes)

}
