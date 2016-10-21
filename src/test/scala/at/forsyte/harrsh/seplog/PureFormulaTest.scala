package at.forsyte.harrsh.seplog

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import at.forsyte.harrsh.models.{Stack, StackBasedEvaluator}
import at.forsyte.harrsh.seplog.indexed.PureFormula
import at.forsyte.harrsh.test.HarrshTest
import at.forsyte.harrsh.test.generators.{PureFormulaGen, StackGens}

/**
  * Created by jkatelaa on 10/7/16.
  */
class PureFormulaTest extends HarrshTest with GeneratorDrivenPropertyChecks {

  val Vars = Set("x","y","z")
  val Vals = Set(1, 2, 3, 4, 5)

  "Pure formulas " should "simplify to formulas with identical evaluation" in {

    forAll(StackGens.stackGen(Vars, Vals), PureFormulaGen.pureFormulaGen(Vars, Vals), minSuccessful(500)) {
      (s: Stack, f: PureFormula) =>
        val simpl = f.simplify
        println(f + " ---> " + simpl + " on " + s)
        StackBasedEvaluator.eval(s, f) should equal(StackBasedEvaluator.eval(s, simpl))
    }

  }

}
