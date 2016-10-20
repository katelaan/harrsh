package slex.test.generators

import org.scalacheck.Gen
import slex.seplog.PtrExpr
import slex.seplog.indexed._

/**
  * Created by jens on 10/9/16.
  */
object PureAtomGens {

  /**
    * Generator for pure atoms containng the given sets of variables and constant values
    */
  def pureAtomGen(vars : Set[String], vals : Set[Int]) : Gen[IndexedPureAtom] = Gen.frequency(
    (1, Gen.const(True())),
    (1, Gen.const(False())),
    (6, atomsWithIntsGen(vars, vals)),
    (4, atomsWithPtrsGen(vars))
  )

  private def atomsWithIntsGen(vars : Set[String], vals : Set[Int]) : Gen[IndexedPureAtom] =
    for {
      l <- SlExprGens.intExprGen(vars, vals)
      r <- SlExprGens.intExprGen(vars, vals)
      a <- inject(l,r)
    } yield a

  private def atomsWithPtrsGen(vars : Set[String]) : Gen[IndexedPureAtom] =
    for {
      l <- SlExprGens.ptrExprGen(vars)
      r <- SlExprGens.ptrExprGen(vars)
      a <- inject(l,r)
    } yield a

  private def inject(l : IntExpr, r : IntExpr) : Gen[IndexedPureAtom] = Gen.oneOf(
    IxEq(l, r),
    IxGT(l, r),
    IxLT(l, r),
    IxLEq(l, r),
    IxGEq(l, r),
    IxNEq(l, r)
  )

  private def inject(l : PtrExpr, r : PtrExpr) : Gen[IndexedPureAtom] = Gen.oneOf(
    PtrEq(l, r),
    PtrNEq(l, r)
  )

}
