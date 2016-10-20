package slex.test.generators

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import slex.models.Stack
import slex.seplog.indexed._
import slex.seplog.PtrExpr
import slex.test.SlexTest

/**
  * This object just samples the various generators.
  * Run to get a feeling for the kind of objects that the generators produce.
  */
class GenSampling extends SlexTest with GeneratorDrivenPropertyChecks {

  val Vars = Set("x","y","z")
  val Vals = Set(1, 2, 3, 4, 5)

  println("Stacks")
  forAll(StackGens.stackGen(Vars, Vals)) {
    (s : Stack) =>
      println(s)
  }

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
    (a : IndexedPureAtom) =>
      println(a)
  }

  println("Formulas")
  forAll(PureFormulaGen.pureFormulaGen(Vars, Vals)) {
    (a : PureFormula) =>
      println(a)
  }


}
