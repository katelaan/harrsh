package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.{ExampleSids, Implicits, TestValues}
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.heapautomata.instances.{ToyExampleAutomata, TrackingAutomata}
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends HarrshTableTest with Implicits with TestValues {

  val inconsistentX1X2 = TrackingInfo.inconsistentTrackingInfo(Seq(x1,x2))

  val Empty = true
  val NonEmpty = false

  val emptinessChecks = Table(
    ("automaton", "sid", "result"),

    /*
     * Has pointer automaton
     */
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSids.Sll, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSids.Dll, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSids.Tree, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSids.Tll, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSids.EmptyLinearPermuter, Empty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSids.NonEmptyLinearPermuter, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSids.NonEmptyBinaryPermuter, NonEmpty),

    /*
     * Tracking automata
      */
    // - Normal tests
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set()), ExampleSids.Sll, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set(x1 =/= x2)), ExampleSids.Sll, Empty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set()), ExampleSids.EmptyLinearPermuter, Empty),
    (TrackingAutomata.singleTargetStateTracking(Set(), Set(x1 =:= x2)), ExampleSids.EmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1,x4), Set()), ExampleSids.Dll, Empty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set()), ExampleSids.Tree, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1,x2), Set(x1 =/= x2)), ExampleSids.Tll, NonEmpty),
    // - Inconsistency checks for tracking
    (TrackingAutomata.singleTargetStateTracking(inconsistentX1X2.alloc, inconsistentX1X2.pure), ExampleSids.NonEmptyBinaryPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(inconsistentX1X2.alloc, inconsistentX1X2.pure), ExampleSids.NonEmptyBinaryPermuter2, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(inconsistentX1X2.alloc, inconsistentX1X2.pure), ExampleSids.NonEmptyBinaryPermuter3, NonEmpty),

    /*
     * SAT automata
     */
    // - on SIDs that produce at least one satisfiable heap
    (TrackingAutomata.satAutomaton, ExampleSids.Sll, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSids.Dll, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSids.Tree, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSids.Tll, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSids.EmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSids.NonEmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSids.NonEmptyBinaryPermuter, NonEmpty),

    /*
     * UNSAT automata
     */
    (TrackingAutomata.satAutomaton, ExampleSids.UnsatSID, Empty),
    // - with consistent SIDs that do not produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton, ExampleSids.Sll, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.Dll, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.Tree, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.Tll, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.EmptyLinearPermuter, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.NonEmptyLinearPermuter, Empty),
    // - with inconsistent SIDs that do produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton, ExampleSids.NonEmptyBinaryPermuter, NonEmpty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.NonEmptyBinaryPermuter2, NonEmpty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.NonEmptyBinaryPermuter3, NonEmpty),
    //- with unsatisfiable SIDs
    (TrackingAutomata.unsatAutomaton, ExampleSids.UnsatSID, NonEmpty),
    (TrackingAutomata.unsatAutomaton, ExampleSids.UnsatSID2, NonEmpty),

    /*
     * Establishment automata
     */
    // - with established data structure SIDs
    (TrackingAutomata.establishmentAutomaton, ExampleSids.Sll, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.Dll, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.Tree, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.Tll, NonEmpty),
    // - with unsat SIDs (Inconsistent heaps are established in our current interpretation)
    (TrackingAutomata.establishmentAutomaton, ExampleSids.UnsatSID, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.UnsatSID2, NonEmpty),
    // - with partially-established SIDs
    (TrackingAutomata.establishmentAutomaton, ExampleSids.OptionallyEstablishedSID, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.OptionallyEstablishedSID2, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.OptionallyEstablishedSID3, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.OptionallyEstablishedSID4, NonEmpty),
    // - non-established SIDs
    (TrackingAutomata.establishmentAutomaton, ExampleSids.NonEstablishedSID, Empty),
    (TrackingAutomata.establishmentAutomaton, ExampleSids.NonEstablishedSID2, Empty),

    /*
     * Non-Establishment automata
     */
    // - with established data structure SIDs
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.Sll, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.Dll, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.Tree, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.Tll, Empty),
    // - with partially-established SIDs
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.OptionallyEstablishedSID, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.OptionallyEstablishedSID2, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.OptionallyEstablishedSID3, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSids.OptionallyEstablishedSID4, NonEmpty),

    /*
     * Reachability automata
     */
    (TrackingAutomata.reachabilityAutomaton(x1, x2), ExampleSids.Sll, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(x1, x4), ExampleSids.Dll, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(x1, nil), ExampleSids.Tree, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(x1, x2), ExampleSids.Tll, NonEmpty),

    /*
     * Garbage-freedom automata
     */
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSids.Sll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSids.Dll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSids.Tree, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSids.Tll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSids.GarbageSll, Empty),

    /*
     * Acyclicity automata
     */
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSids.Sll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSids.Dll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSids.Tree, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSids.Tll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSids.CyclicSll, Empty),

    /*
     * Small circuit examples
     */
    (TrackingAutomata.satAutomaton, "succ-circuit02.defs".load(), NonEmpty),
    (TrackingAutomata.satAutomaton, "succ-rec02.defs".load(), NonEmpty),
    (TrackingAutomata.unsatAutomaton, "succ-circuit02.defs".load(), NonEmpty),
    (TrackingAutomata.unsatAutomaton, "succ-rec02.defs".load(), NonEmpty)

  )

  property("On-the-fly emptiness checking") {

    forAll(emptinessChecks) {
      (automaton, sid, result) =>

        Given(sid + "\n and the automaton '" + automaton.description)
        Then("The emptiness check should return " + result)

        info("Testing emptiness for refinement of " + sid + "\n with the automaton '" + automaton.description + "'; expected result: " + result)
        RefinementAlgorithms.onTheFlyRefinementWithEmptinessCheck(sid, automaton, reportProgress = true) should be(result)
    }

  }

//  // Note: To try a single test case, uncomment the following
//  val (extraAUT, extraSID, extraRes) = (TrackingAutomata.establishmentAutomaton, ExampleSIDs.NonEstablishedSID, Empty)
//
//  println("Testing emptiness for refinement of " + extraSID + "\n with the automaton '" + extraAUT.description + "'; expected result: " + extraRes)
//  RefinementAlgorithms.onTheFlyRefinementWithEmptinessCheck(extraSID, extraAUT, reportProgress = true) should be(extraRes)
//  println("########")

}
