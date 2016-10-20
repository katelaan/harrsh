package slex.main

import slex.heapautomata.AutomatonTask

/**
  * Created by jkatelaa on 10/20/16.
  */
case class TaskConfig(fileName : String, decisionProblem : AutomatonTask, expectedResult : Option[Boolean]) {

}
