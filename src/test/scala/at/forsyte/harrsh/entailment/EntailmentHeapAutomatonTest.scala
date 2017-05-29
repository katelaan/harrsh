package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.learning.EntailmentAutomatonLearning
import at.forsyte.harrsh.test.HarrshTest
import at.forsyte.harrsh.Implicits._
import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.seplog.inductive.SID

/**
  * Created by jkatelaa on 5/29/17.
  */
class EntailmentHeapAutomatonTest extends HarrshTest {

  behavior of "entailment heap auotmaton"

  it should "solve entailment checking for ASLLs" in {

    val sid = "sll-acyc.sid".load()
    val numFV = 2
    val (asllTable, _) = EntailmentAutomatonLearning.learnAutomaton(sid, numFV, assumeAsymmetry = false, useSimpleLearning = true, reportProgress = false)
    val automaton = EntailmentHeapAutomaton.fromObservationTable(numFV, asllTable)

    val sidCyc = "sll.sid".load()

    def decide(sid : SID) : Boolean = EntailmentHeapAutomaton.decideEntailment(sid, automaton, TestValues.DefaultTestTimeout, verbose = true, reportProgress = true)

    decide(sid) shouldBe true

  }

}
