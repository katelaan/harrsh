package slex.main

import slex.algs.MDEC
import slex.main.main.examples.{SymbolicHeapExamples, UFExample}
import slex.slsyntax.SepLogAxioms
import slex.smtinteraction.{NaiveZ3Wrapper, SmtWrapper}

/**
  * Created by jkatelaa on 9/30/16.
  */
object Slex {

  def main(args : Array[String]) = {
    println("Slexy")
    val wrapper : SmtWrapper = new NaiveZ3Wrapper(None)
    val example = UFExample.Example
    println("Will run the following example:")
    println(example.mkString("\n"))
    println("Running Z3 now...")
    val res = wrapper.runSmtQuery(example)
    println(res)

    println("Look, we can write SL definitions! With indices! Here's one for list segments:")
    println(SepLogAxioms.LSegDef)

    println("We also support the restriction to symbolic heaps such as")
    println(SymbolicHeapExamples.SingleList)
    println(SymbolicHeapExamples.SplitList)
    println(SymbolicHeapExamples.LassoList)

    println("Let's test the model-driven entailment checker...")
    val res2= new MDEC(wrapper).prove(SymbolicHeapExamples.Entailment1Left.get, SymbolicHeapExamples.Entailment1Right.get)
    println("Result: " + res2)
  }

}
