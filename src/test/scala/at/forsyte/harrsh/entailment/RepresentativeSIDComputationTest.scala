package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.{ExampleSIDs}
import at.forsyte.harrsh.test.HarrshTest
import at.forsyte.harrsh.Implicits._


/**
  * Created by jens on 5/3/17.
  */
class RepresentativeSIDComputationTest extends HarrshTest {

  behavior of "Computation of SID for representatives"

  it should "allow ground entailments for the underlying representative" in {

    val sh = "x1 -> x3 * x4 -> x2".parse
    val res = RepresentativeSIDComputation.adaptSIDToRepresentative(ExampleSIDs.Sll, sh)
    println(res)
    assert(GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(sh, res.callToStartPred, res))

  }

}
