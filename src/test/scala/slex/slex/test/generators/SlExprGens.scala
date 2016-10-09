package slex.slex.test.generators

import org.scalacheck.Gen
import slex.Sorts.Location
import slex.seplog._

/**
  * Created by jkatelaa on 10/7/16.
  */
object SlExprGens {

  def intExprGen(vars : Set[String], vals : Set[Int]) : Gen[IntExpr] = intExprGen(vars.toSeq, vals.toSeq)

  def ptrExprGen(vars : Set[String]) : Gen[PtrExpr] = ptrExprGen(vars.toSeq)

  /*
   * INT EXPRS
   */

  private def intConst(vals : Seq[Int]) : Gen[IntExpr] = Gen.oneOf(vals) map IntConst

  private def intVar(vars : Seq[String]) : Gen[IntExpr] = Gen.oneOf(vars) map IntVar

  private def intPlus(vars : Seq[String], vals : Seq[Int]) : Gen[IntExpr] =
    for {
      left <- intExprGen(vars, vals)
      right <- intExprGen(vars, vals)
    } yield Plus(left, right)

  private def intMinus(vars : Seq[String], vals : Seq[Int]) : Gen[IntExpr] =
    for {
      left <- intExprGen(vars, vals)
      right <- intExprGen(vars, vals)
    } yield Minus(left, right)

  private def intExprGen(vars : Seq[String], vals : Seq[Int]) : Gen[IntExpr] = Gen.frequency(
    (3, intConst(vals)),
    (3, intVar(vars)),
    (2, intPlus(vars, vals)),
    (2, intMinus(vars, vals))
  )

  /*
   * PTR EXPRS
   */

  private def nullPtr : Gen[PtrExpr] = Gen.const(NullPtr())
  private def varPtr(vars: Seq[String]) : Gen[PtrExpr] = Gen.oneOf(vars) map PtrVar

  private def ptrExprGen(vars: Seq[String]): Gen[PtrExpr] = Gen.frequency(
    (1, nullPtr),
    (5, varPtr(vars))
  )

}
