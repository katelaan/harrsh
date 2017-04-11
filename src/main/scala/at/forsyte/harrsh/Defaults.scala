package at.forsyte.harrsh

/**
  * Created by jkatelaa on 9/30/16.
  */
object Defaults {

  val PathToZ3 = System.getenv().get("HOME") + "/z3"

  val PathsToExamples = Seq("examples/datastructures", "examples/symbolicheaps", "examples/models/", "examples/cyclist")

  var reportProgress = false

}
