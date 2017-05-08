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

  it should "compute just one non-bottom class for SLLs" in {

    val sid = "sll.sid".load()
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)

    obs.entries.size shouldEqual 1
    assert(obs.entries.head.isFinal)
    assert("x1 -> x2".parse.isA(obs.entries.head.repSid))
  }

  it should "compute three classes for acyclic SLLs" in {

    val sid = "sll-acyc.sid".load()
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true)
    printCase(sid, obs, log)

//    obs.entries.size shouldEqual 1
//    assert(obs.entries.head.isFinal)
//    assert("x1 -> x2".parse.isA(obs.entries.head.repSid))

  }

  it should "merge all classes" in {

    val sid =
      """
        |merge <= emp : { x1 = x2 } ;
        |merge <= x1 -> (y1, null) * merge(y1, x2) ;
        |merge <= x1 -> (null, y1) * merge(y1, x2)
      """.stripMargin.parseSID
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true, 4)
    printCase(sid, obs, log)

  }

  it should "merge some classes" in {

    val sid =
      """
        |overlap <= x1 -> (null, y1, null) * y1 -> (null, x2, null) ;
        |overlap <= x1 -> (null, y1, null) * y1 -> (null, null, x2) ;
        |overlap <= x1 -> (null, null, y1) * y1 -> (null, null, x2) ;
        |overlap <= x1 -> (null, y1, y1) * y1 -> (null, null, x2) ;
        |overlap <= x1 -> (null, y1, y1) * y1 -> (null, x2, x2) ;
        |overlap <= x1 -> (y1, null, null) * overlap(y1, x2)
      """.stripMargin.parseSID
    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true, 4)
    printCase(sid, obs, log)

  }

//  it should "not crash on trees" in {
//
//    val sid = "tree.sid".load
//    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true, 4)
//    printCase(sid, obs, log)
//
//  }

//  it should "not crash on tlls" in {
//
//    val sid = "tll.sid".load
//    val (obs, log) = EntailmentAutomatonLearning.learnAutomaton(sid, 2, true, 4)
//    printCase(sid, obs, log)
//
//  }

  private def printCase(sid : SID, obs : ObservationTable, log : EntailmentLearningLog) : Unit = {
    println()
    IOUtils.printLinesOf('#', 3)
    println(sid)
    println(log)
    println(obs)
    IOUtils.printLinesOf('/', 1)
    println()
  }

}
