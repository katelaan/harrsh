package slex.seplog

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import slex.models.{Stack, StackBasedEvaluator}
import slex.test.SlexTest
import slex.test.generators.{PureFormulaGen, StackGens}

/**
  * Created by jkatelaa on 10/7/16.
  */
class PureFormulaTest extends SlexTest with GeneratorDrivenPropertyChecks {

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
