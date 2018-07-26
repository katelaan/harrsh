package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.{ExampleSIDs, TestValues}
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.heapautomata.instances.{ToyExampleAutomata, TrackingAutomata}
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.test.HarrshTableTest
import at.forsyte.harrsh.Implicits._

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends HarrshTableTest with TestValues {

  val inconsistentX1X2 = TrackingInfo.inconsistentTrackingInfo(Seq(x1,x2))

  val Empty = true
  val NonEmpty = false

  val emptinessChecks = Table(
    ("automaton", "sid", "result"),

    /*
     * Has pointer automaton
     */
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSIDs.Sll, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSIDs.Dll, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSIDs.Tree, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSIDs.Tll, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSIDs.EmptyLinearPermuter, Empty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSIDs.NonEmptyLinearPermuter, NonEmpty),
    (ToyExampleAutomata.hasPointerAutomaton(), ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),

    /*
     * Tracking automata
      */
    // - Normal tests
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set()), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set(x1 =/= x2)), ExampleSIDs.Sll, Empty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set()), ExampleSIDs.EmptyLinearPermuter, Empty),
    (TrackingAutomata.singleTargetStateTracking(Set(), Set(x1 =:= x2)), ExampleSIDs.EmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1,x4), Set()), ExampleSIDs.Dll, Empty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1), Set()), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(Set(x1,x2), Set(x1 =/= x2)), ExampleSIDs.Tll, NonEmpty),
    // - Inconsistency checks for tracking
    (TrackingAutomata.singleTargetStateTracking(inconsistentX1X2.alloc, inconsistentX1X2.pure), ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(inconsistentX1X2.alloc, inconsistentX1X2.pure), ExampleSIDs.NonEmptyBinaryPermuter2, NonEmpty),
    (TrackingAutomata.singleTargetStateTracking(inconsistentX1X2.alloc, inconsistentX1X2.pure), ExampleSIDs.NonEmptyBinaryPermuter3, NonEmpty),

    /*
     * SAT automata
     */
    // - on SIDs that produce at least one satisfiable heap
    (TrackingAutomata.satAutomaton, ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSIDs.Tll, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSIDs.EmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSIDs.NonEmptyLinearPermuter, NonEmpty),
    (TrackingAutomata.satAutomaton, ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),

    /*
     * UNSAT automata
     */
    (TrackingAutomata.satAutomaton, ExampleSIDs.UnsatSID, Empty),
    // - with consistent SIDs that do not produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.Sll, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.Dll, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.Tree, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.Tll, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.EmptyLinearPermuter, Empty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.NonEmptyLinearPermuter, Empty),
    // - with inconsistent SIDs that do produce unsatisfiable heaps
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.NonEmptyBinaryPermuter, NonEmpty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.NonEmptyBinaryPermuter2, NonEmpty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.NonEmptyBinaryPermuter3, NonEmpty),
    //- with unsatisfiable SIDs
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.UnsatSID, NonEmpty),
    (TrackingAutomata.unsatAutomaton, ExampleSIDs.UnsatSID2, NonEmpty),

    /*
     * Establishment automata
     */
    // - with established data structure SIDs
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.Tll, NonEmpty),
    // - with unsat SIDs (Inconsistent heaps are established in our current interpretation)
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.UnsatSID, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.UnsatSID2, NonEmpty),
    // - with partially-established SIDs
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID2, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID3, NonEmpty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID4, NonEmpty),
    // - non-established SIDs
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.NonEstablishedSID, Empty),
    (TrackingAutomata.establishmentAutomaton, ExampleSIDs.NonEstablishedSID2, Empty),

    /*
     * Non-Establishment automata
     */
    // - with established data structure SIDs
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.Sll, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.Dll, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.Tree, Empty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.Tll, Empty),
    // - with partially-established SIDs
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID2, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID3, NonEmpty),
    (TrackingAutomata.nonEstablishmentAutomaton, ExampleSIDs.OptionallyEstablishedSID4, NonEmpty),

    /*
     * Reachability automata
     */
    (TrackingAutomata.reachabilityAutomaton(x1, x2), ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(x1, x4), ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(x1, nil), ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.reachabilityAutomaton(x1, x2), ExampleSIDs.Tll, NonEmpty),

    /*
     * Garbage-freedom automata
     */
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSIDs.Tll, NonEmpty),
    (TrackingAutomata.garbageFreedomAutomaton, ExampleSIDs.GarbageSll, Empty),

    /*
     * Acyclicity automata
     */
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSIDs.Sll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSIDs.Dll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSIDs.Tree, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSIDs.Tll, NonEmpty),
    (TrackingAutomata.weakAcyclicityAutomaton, ExampleSIDs.CyclicSll, Empty),

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
