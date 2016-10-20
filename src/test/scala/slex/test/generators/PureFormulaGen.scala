package slex.test.generators

import org.scalacheck.Gen
import slex.seplog.indexed.{PureAnd, PureFormula, PureNeg, PureOr}

/**
  * Created by jens on 10/9/16.
  */
object PureFormulaGen {

  /**
    * Generator for pure formulas containing the given vars and vals
    */
  def pureFormulaGen(vars : Set[String], vals : Set[Int]) : Gen[PureFormula] = Gen.frequency(
    (7, PureAtomGens.pureAtomGen(vars, vals)),
    (2, Gen.lzy(negGen(vars, vals))),
    (2, Gen.lzy(andGen(vars, vals))),
    (2, Gen.lzy(orGen(vars, vals)))
  )

  private def negGen(vars : Set[String], vals : Set[Int]) : Gen[PureFormula] = pureFormulaGen(vars, vals) map PureNeg

  private def andGen(vars : Set[String], vals : Set[Int]) : Gen[PureFormula] =
    for {
      l <- pureFormulaGen(vars, vals)
      r <- pureFormulaGen(vars, vals)
    } yield PureAnd(l, r)

  private def orGen(vars : Set[String], vals : Set[Int]) : Gen[PureFormula] =
    for {
      l <- pureFormulaGen(vars, vals)
      r <- pureFormulaGen(vars, vals)
    } yield PureOr(l, r)

}
