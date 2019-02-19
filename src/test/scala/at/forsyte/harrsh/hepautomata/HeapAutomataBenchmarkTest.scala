package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.main.MainIO
import at.forsyte.harrsh.refinement.DecisionProcedures
import at.forsyte.harrsh.test.HarrshTest

import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by jens on 4/21/17.
  */
class HeapAutomataBenchmarkTest extends HarrshTest {

  // Loads benchmark suite from examples + fails if results differ from expected results

  behavior of "Heap automata"

  they should "reproduce the benchmark suite" in {

    val tasks = MainIO.readTasksFromFile("examples/test-suite-benchmarks.bms")

    for (query <- tasks) {

      withClue(s"For task $query:") {
        val result = DecisionProcedures.decideInstance(query, Duration(10, SECONDS), verbose = false, reportProgress = false)
        result.isEmpty shouldEqual !query.status.toBoolean.get
      }

    }

  }

}
