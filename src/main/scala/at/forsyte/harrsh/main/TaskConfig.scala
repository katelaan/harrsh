package at.forsyte.harrsh.main

import at.forsyte.harrsh.refinement.AutomatonTask

/**
  * Created by jkatelaa on 10/20/16.
  */
case class TaskConfig(fileName : String, decisionProblem : AutomatonTask, expectedResult : Option[Boolean]) {

  override def toString = {
    val resString = expectedResult match {
      case Some(x) => x
      case None => "???"
    }

    fileName + "; " + decisionProblem + "; " + resString
  }

}

object TaskConfig {

  def fromString(s : String) : Option[TaskConfig] = {

    // TODO Use Validation instead

    val parts = s.split(";").map(_.trim)

    val res = if (parts.length == 3) {
      val fileName = parts(0)
      val optDecProb = AutomatonTask.fromString(parts(1))
      val expRes = parts(2) match {
        case "false" => Some(false)
        case "true" => Some(true)
        case _ => None
      }

      optDecProb map (TaskConfig(fileName, _, expRes))
    } else {
      None
    }

    if (res.isEmpty) {
      println("Error: Failed parsing '" + s + "'")
    }

    res
  }

}
