package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.test.HarrshTest
import at.forsyte.harrsh.Implicits._
import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.util.IOUtils

/**
  * Created by jens on 5/3/17.
  */
class EntailmentAutomatonLearningTest extends HarrshTest {

  behavior of "entailment automaton learning"

  it should "learn SLL entailment checking" in {

    val sid = "sll.sid".load()
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)

    obs.numClasses shouldEqual 1
    obs.finalClasses.size shouldEqual 1
    assert(automatonAccepts("x1 -> x2", obs))
    assert(automatonAccepts("x1 -> y1 * y1 -> x2", obs))
    assert(automatonAccepts("x1 -> y1 * y1 -> y3 * y3 -> y5 * y5 -> y6 * y6 -> y4 * y4 -> y2 * y2 -> x2", obs))
    assert(automatonRejects("emp", obs)) // Rejected because emp gets its own equivalence class in asymmetric SIDs
    assert(automatonRejects("x2 -> x1", obs)) // Rejected because we only learn the classes for a fixed variable ordering
    IOUtils.printLinesOf('#', 1)
  }

  it should "learn ACYC SLL entailment checking" in {

    val sid = "sll-acyc.sid".load()
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)

    obs.numClasses shouldEqual 3
    obs.finalClasses.size shouldEqual 1
    assert(automatonRejects("emp", obs))
    assert(automatonRejects("emp : {x1 != x2}", obs))
    assert(automatonRejects("x1 -> x2", obs))
    assert(automatonAccepts("emp : {x1 = x2}", obs))
    assert(automatonAccepts("x1 -> x2 : {x1 != x2}", obs))
    assert(automatonAccepts("x1 -> y1 * y1 -> x2 : {x1 != x2}", obs))
    IOUtils.printLinesOf('#', 1)
  }

  it should "merge classes with identical extensions" in {

    // For the following SID, we should learn a single equivalence class:
    // x1 -> (y1, null) and x1 -> (null, y1) are (minimal) representatives of the same class;
    // the table entries for these representative should thus be merged at the end of the iteration

    val sid =
      """
        |merge <= emp : { x1 = x2 } ;
        |merge <= x1 -> (y1, null) * merge(y1, x2) ;
        |merge <= x1 -> (null, y1) * merge(y1, x2)
      """.stripMargin.parseSID
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)

    obs.numClasses shouldEqual 1
    obs.finalClasses.size shouldEqual 1
    assert(automatonAccepts("emp : {x1 = x2}", obs))
    assert(automatonAccepts("x1 -> (y1,null) * y1 -> (null,y2) * y2 -> (y3,null) : {y3 = x2}", obs))
    assert(automatonAccepts("x1 -> (y1,null) * y1 -> (null,y2) * y2 -> (x2,null)", obs))
    IOUtils.printLinesOf('#', 1)
  }

  it should "learn SIDs with multiple minimal representatives per class" in {

    // For the following SID, we should learn a single equivalence class:
    // x1 -> (y1, null) and x1 -> (null, y1) are (minimal) representatives of the same class;
    // the table entries for these representative should thus be merged at the end of the iteration

    val sid =
      """
        |multi <= x1 -> (x2, null) ;
        |multi <= x1 -> (null, x2) ;
        |multi <= x1 -> (y1, null) * multi(y1, x2) ;
        |multi <= x1 -> (null, y1) * multi(y1, x2)
      """.stripMargin.parseSID
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)

    obs.numClasses shouldEqual 1
    obs.finalClasses.size shouldEqual 1
    obs.finalClasses.head.reps.size shouldEqual 2
    assert(automatonAccepts("x1 -> (y1,null) * y1 -> (null,y2) * y2 -> (y3,null) : {y3 = x2}", obs))
    assert(automatonAccepts("x1 -> (y1,null) * y1 -> (null,y2) * y2 -> (x2,null)", obs))
    IOUtils.printLinesOf('#', 1)
  }

  it should "learn SIDs with two overlapping classes" in {

    // In the following SID
    // - the extensions of classes for x1 -> (null, x2, null) and x1 -> (null, null, x2) overlap
    // - the extensions of classes for for x1 -> (null, x2) and x1 -> (x2, null) overlap
    // This should trigger some splitting of entries and in the end result in 6 classes, tested for below

    val sid =
      """
        |overlap <= x1 -> (null, y1, null) * y1 -> (null, x2) ;
        |overlap <= x1 -> (null, y1, null) * y1 -> (x2, null) ;
        |overlap <= x1 -> (null, null, y1) * y1 -> (x2, null) ;
        |overlap <= x1 -> (y1, null, null) * overlap(y1, x2)
      """.stripMargin.parseSID
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)

    obs.numClasses shouldEqual 6
    obs.finalClasses.size shouldEqual 1

    // Check that all the expected classes are there
    val representatives = Seq(
      // Class for the recursive rule (can take any base rule body as suffix)
      "x1 -> (x2, null, null)",
      // Classes for the left pointers of the base rules
      "x1 -> (null, x2, null)",
      "x1 -> (null, null, x2)",
      // Classes for the right pointers of the base rules
      "x1 -> (null, x2)",
      "x1 -> (x2, null)",
      // Accepting class, represented by all base rule bodies
      "∃y1 . y1 ↦ (null, x2) * x1 ↦ (null, y1, null)",
      "∃y1 . y1 ↦ (x2, null) * x1 ↦ (null, y1, null)",
      "∃y1 . y1 ↦ (x2, null) * x1 ↦ (null, null, y1)"
    )

    for (rep <- representatives) {
      assert(hasClassRepresentedBy(obs, rep))
    }

    assert(automatonAccepts("∃y1 . y1 ↦ (x2, null) * x1 ↦ (null, y1, null)", obs))
    assert(automatonAccepts("∃y1 ∃y2 ∃y3 . x1 ↦ (y1, null, null) * y1 ↦ (y2, null, null) * y2 ↦ (null, y3, null) * y3 ↦ (x2, null)", obs))
    assert(automatonRejects("x1 -> (x2, null, null)", obs))
    assert(automatonRejects("x1 -> (null, x2, null)", obs))
    assert(automatonRejects("x1 -> (x2, null)", obs))
    assert(automatonRejects("x1 -> (y1, null, null) * y1 -> (null, x2, null)", obs))
      
    IOUtils.printLinesOf('#', 1)
  }

  ignore should "learn SIDs with three overlapping classes" in {

    val sid =
      """
        |overlap <= x1 -> (null, y1, null) * y1 -> (null, x2) ;
        |overlap <= x1 -> (null, y1, null) * y1 -> x2 ;
        |overlap <= x1 -> (null, null, y1) * y1 -> x2 ;
        |overlap <= x1 -> (null, y1, y1) * y1 -> x2 ;
        |overlap <= x1 -> (null, y1, y1) * y1 -> (x2, null) ;
        |overlap <= x1 -> (y1, null, null) * overlap(y1, x2)
      """.stripMargin.parseSID
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)
    IOUtils.printLinesOf('#', 1)
  }

  ignore should "learn SIDs with overlaid classes" in {

    // Here we have a more complicated overlapping pattern, since pointers of the same type occur both on the left
    // and on the right of the two-pointer base-rules

    val sid =
      """
        |overlap <= x1 -> (null, y1, null) * y1 -> (null, x2, null) ;
        |overlap <= x1 -> (null, y1, null) * y1 -> (null, null, x2) ;
        |overlap <= x1 -> (null, null, y1) * y1 -> (null, null, x2) ;
        |overlap <= x1 -> (null, y1, y1) * y1 -> (null, null, x2) ;
        |overlap <= x1 -> (null, y1, y1) * y1 -> (null, x2, x2) ;
        |overlap <= x1 -> (y1, null, null) * overlap(y1, x2)
      """.stripMargin.parseSID
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)
    IOUtils.printLinesOf('#', 1)
  }

  ignore should "not crash on trees" in {

    val sid = "tree.sid".load
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true, 4)
    printCase(sid, obs, log)
    IOUtils.printLinesOf('#', 1)
  }

  ignore should "not crash on tlls" in {

    val sid = "tll.sid".load
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true, 4)
    printCase(sid, obs, log)
    IOUtils.printLinesOf('#', 1)
  }

  private def automatonAccepts(sh : String, obs : ObservationTable) : Boolean = {
    println("Automaton should accept " + sh)
    obs.accepts(sh.parse, verbose = true)
  }
  private def automatonRejects(sh : String, obs : ObservationTable) : Boolean = {
    println("Automaton should reject " + sh)
    obs.rejects(sh.parse, verbose = true)
  }

  private def hasClassRepresentedBy(obs : ObservationTable, sh : String) = {
    obs.entries.flatMap(_.reps).contains(sh.parse)
  }

  private def printCase(sid : SID, obs : ObservationTable, log : EntailmentLearningLog) : Unit = {
    println()
    IOUtils.printLinesOf('#', 3)
    println(sid)
    IOUtils.printLinesOf('-', 1)
    println(log)
    IOUtils.printLinesOf('-', 1)
    println(obs)
    IOUtils.printLinesOf('/', 1)
    println()
  }

}
