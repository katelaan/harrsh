package slex.slex.seplog

import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import slex.SlexTest
import slex.seplog.{IntExpr, PtrExpr, PureAtom, PureFormula}
import slex.slex.test.generators.{PureAtomGens, PureFormulaGen, SlExprGens}

/**
  * Created by jkatelaa on 10/7/16.
  */
class PureAtomTest extends SlexTest with GeneratorDrivenPropertyChecks with Matchers {

  val Vars = Set("x","y","z")
  val Vals = Set(1, 2, 3, 4, 5)

  println("Int Expressions")
  forAll(SlExprGens.intExprGen(Vars, Vals)) {
    (a : IntExpr) =>
      println(a)
  }

  println("Ptr Expressions")
  forAll(SlExprGens.ptrExprGen(Vars)) {
    (a : PtrExpr) =>
      println(a)
  }

  println("Atoms")
  forAll(PureAtomGens.pureAtomGen(Vars, Vals)) {
    (a : PureAtom) =>
      println(a)
  }

  println("Formulas")
  forAll(PureFormulaGen.pureFormulaGen(Vars, Vals)) {
    (a : PureFormula) =>
      println(a)
  }

}
