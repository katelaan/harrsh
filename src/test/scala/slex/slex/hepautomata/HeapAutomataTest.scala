package slex.slex.hepautomata

import slex.SlexTest
import slex.heapautomata.{ExampleSIDs, RefinementAlgorithms, ExampleAutomata}

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends SlexTest {

  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.Sll, ExampleAutomata.HasPointerAutomaton) should be (false)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.EmptyLinearPermuter, ExampleAutomata.HasPointerAutomaton) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyLinearPermuter, ExampleAutomata.HasPointerAutomaton) should be (false)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyBinaryPermuter, ExampleAutomata.HasPointerAutomaton) should be (false)

}
