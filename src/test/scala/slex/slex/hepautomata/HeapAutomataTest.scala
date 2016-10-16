package slex.slex.hepautomata

import slex.SlexTest
import slex.heapautomata.{ExampleSIDs, RefinementAlgorithms, ToyExampleAutomata}

/**
  * Created by jens on 10/15/16.
  */
class HeapAutomataTest extends SlexTest {

  println("Testing emptiness check for the HasPointerAutomaton")

  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.Sll, ToyExampleAutomata.HasPointerAutomaton) should be (false)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.EmptyLinearPermuter, ToyExampleAutomata.HasPointerAutomaton) should be (true)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyLinearPermuter, ToyExampleAutomata.HasPointerAutomaton) should be (false)
  RefinementAlgorithms.onTheFlyEmptinessCheck(ExampleSIDs.NonEmptyBinaryPermuter, ToyExampleAutomata.HasPointerAutomaton) should be (false)

}
