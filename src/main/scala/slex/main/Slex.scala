package slex.main

import slex.main.main.examples.UFExample
import slex.slex.slsyntax.SepLogAxioms
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
  }

}
