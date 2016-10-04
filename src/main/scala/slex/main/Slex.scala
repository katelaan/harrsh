package slex.main

import slex.algs.MDEC
import slex.main.main.examples.{SymbolicHeapExamples, UFExample}
import slex.slsyntax.SepLogAxioms
import slex.smtinteraction.{NaiveZ3Wrapper, SmtWrapper, Z3ResultParser}

/**
  * Created by jkatelaa on 9/30/16.
  */
object Slex {

  def main(args : Array[String]) = {
    println("This is slexy!")

    println("Let's test the Z3 result parser")

    mdecExample
  }

  def mdecExample() : Unit = {
    println("Let's test the model-driven entailment checker...")
    val wrapper: SmtWrapper = new NaiveZ3Wrapper(None)
    val res = new MDEC(wrapper).prove(SymbolicHeapExamples.Entailment2Left.get, SymbolicHeapExamples.Entailment2Right.get)
    println("Result: " + res)
  }

  private def callSmtExample() : Unit = {
    val wrapper : SmtWrapper = new NaiveZ3Wrapper(None)
    val example = UFExample.Example
    println("Will run the following example:")
    println(example.mkString("\n"))
    println("Running Z3 now...")
    val res = wrapper.runSmtQuery(example)
    println(res)
  }



  private def formulaExamples() : Unit = {
    println("Look, we can write SL definitions! With indices! Here's one for list segments:")
    println(SepLogAxioms.LSegDef)

    println("We also support the restriction to symbolic heaps such as")
    println(SymbolicHeapExamples.SingleList)
    println(SymbolicHeapExamples.SplitList)
    println(SymbolicHeapExamples.LassoList)
  }

}
