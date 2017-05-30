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

    def decide(sid : SID) : Boolean = EntailmentHeapAutomaton.decideEntailment(sid, automaton, TestValues.DefaultTestTimeout, verbose = true, reportProgress = true)

    // The SID should of course entail itself
    decide(sid) shouldBe true
    // Since standard lists can be cyclic, the entailment should be false
    val sidCyc = "sll.sid".load()
    decide(sidCyc) shouldBe false

    // Individual acyclic lists should also be accepted
    val acycInstance = "x1 -> x2 : {x2 != x1}".parse.toSid
    decide(acycInstance) shouldBe true
    val acycInstance2 = "x1 -> y1 * y1 -> x2 : {x2 != x1, x2 != y1}".parse.toSid
    decide(acycInstance2) shouldBe true
    val acycInstance3 = "x1 -> y1 * y2 -> y3 * y1 -> y2 * y3 -> x2 : {x2 != x1, x2 != y1, x2 != y2, x2 != y3}".parse.toSid
    decide(acycInstance3) shouldBe true

    // Cyclic instances should be rejected
    val stronglyCycInstance = "x1 -> y1 * y2 -> y3 * y1 -> y2 * y3 -> x1".parse.toSid
    decide(stronglyCycInstance) shouldBe false
    val stronglyCycInstance2 = "x1 -> y1 * y2 -> y3 * y1 -> y2 * y3 : { y3 = x1 }".parse.toSid
    decide(stronglyCycInstance2) shouldBe false

    // Chaining acylic lists together should work under the appropriate side conditions
    val twoLists = "asll(x1, y1) * asll(y1, x2) : {x1 != y1, x1 != x2, y1 != x2}".parse.toSid(sid)
    decide(twoLists) shouldBe true
    val twoListsWithoutPure = "asll(x1, y1) * asll(y1, x2)".parse.toSid(sid)
    decide(twoListsWithoutPure) shouldBe false

    // Chaining possibly cyclic lists together should work if the side conditions enforce acyclicity
    // FIXME Make this work
    //val twoListsCyc = "sll(x1, y1) * sll(y1, x2) : {x1 != y1, x1 != x2, y1 != x2}".parse.toSid(sidCyc)
    //decide(twoListsCyc) shouldBe true

  }

}
