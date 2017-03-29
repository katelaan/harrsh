package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.ExampleSIDs
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.pure.EqualityUtils.mkPure
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.heapautomata.instances.{ToyExampleAutomata, TrackingAutomata}
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.inductive.nil
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends HarrshTableTest {

  val inconsistent2 = TrackingInfo.inconsistentTrackingInfo(2)

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
    // - Normal tests
    (TrackingAutomata.singleTargetStateTracking(3, Set(mkVar(1)), mkPure()), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(2, Set(mkVar(1)), mkPure((1, 2, NonEmpty))), ExampleSIDs.Sll, Empty),
    (TrackingAutomata.singleTargetStateTracking(2, Set(mkVar(1)), mkPure()), ExampleSIDs.EmptyLinearPermuter, Empty),
    (TrackingAutomata.singleTargetStateTracking(2, Set(), mkPure((1, 2, Empty))), ExampleSIDs.EmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(4, Set(mkVar(1),mkVar(4)), mkPure()), ExampleSIDs.Dll, Empty),
    (TrackingAutomata.singleTargetStateTracking(1, Set(mkVar(1)), mkPure()), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(3, Set(mkVar(1),mkVar(2)), mkPure((1,2,NonEmpty))), ExampleSIDs.Tll, NonEmpty),
    // - Inconsistency checks for tracking
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2.alloc, inconsistent2.pure), ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2.alloc, inconsistent2.pure), ExampleSIDs.NonEmptyBinaryPermuter2, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(2, inconsistent2.alloc, inconsistent2.pure), ExampleSIDs.NonEmptyBinaryPermuter3, NonEmpty),

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
    (TrackingAutomata.nonEstablishmentAutomaton(2), ExampleSIDs.OptionallyEstablishedSID4, NonEmpty),

    /*
     * Reachability automata
     */
    (TrackingAutomata.reachabilityAutomaton(2, mkVar(1), mkVar(2)), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(4, mkVar(1), mkVar(4)), ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(1, mkVar(1), nil), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(3, mkVar(1), mkVar(2)), ExampleSIDs.Tll, NonEmpty),

    /*
     * Garbage-freedom automata
     */
    (TrackingAutomata.garbageFreedomAutomaton(2), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton(4), ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton(1), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton(3), ExampleSIDs.Tll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton(3), ExampleSIDs.GarbageSll, Empty),

    /*
     * Acyclicity automata
     */
    (TrackingAutomata.weakAcyclicityAutomaton(2), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton(4), ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton(1), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton(3), ExampleSIDs.Tll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton(2), ExampleSIDs.CyclicSll, Empty)
    
  )

  property("On-the-fly emptiness checking") {

    forAll(emptinessChecks) {
      (automaton, sid, result) =>

        Given(sid + "\n and the automaton '" + automaton.description)
        Then("The emptiness check should return " + result)

        println("#"*80)
        println("Testing emptiness for refinement of " + sid + "\n with the automaton '" + automaton.description + "'; expected result: " + result)
        RefinementAlgorithms.onTheFlyRefinementWithEmptinessCheck(sid, automaton, reportProgress = true) should be(result)
        println()
    }

  }

  // Note: To try a single test case, comment out the following
//  val (extraAUT, extraSID, extraRes) = (TrackingAutomata.reachabilityAutomaton(2, mkVar(1), mkVar(2)), ExampleSIDs.Sll, NonEmpty)
//
//  println("Testing emptiness for refinement of " + extraSID + "\n with the automaton '" + extraAUT.description + "'; expected result: " + extraRes)
//  RefinementAlgorithms.onTheFlyRefinementWithEmptinessCheck(extraSID, extraAUT, reportProgress = true) should be(extraRes)

}
