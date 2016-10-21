package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.smtinteraction.SmtWrapper

/**
  * Created by jkatelaa on 9/30/16.
  */
object MDECSampleCalls {

  def mdecExample() : Unit = {
    println("Let's test the model-driven entailment checker...")
    SmtWrapper.withZ3 { z3 =>
      val res = MDEC(z3).prove(IndexedSymbolicHeapExamples.PaperExampleEntailmentLeft, IndexedSymbolicHeapExamples.PaperExampleEntailmentRight)
      println("Result: " + res)
    }
  }

  private def callSmtExample() : Unit = {
    SmtWrapper.withZ3 { z3 =>
      val example = UFExample.Example
      println("Will run the following example:")
      println(example.mkString("\n"))
      println("Running Z3 now...")
      z3.restart()
      z3.addCommands(example)
      val res = z3.computeModel()
      println(res)
    }
  }



  private def formulaExamples() : Unit = {
    println("We also support the restriction to symbolic heaps such as")
    println(IndexedSymbolicHeapExamples.SingleList)
    println(IndexedSymbolicHeapExamples.SplitList)
    println(IndexedSymbolicHeapExamples.LassoList)
  }

}
