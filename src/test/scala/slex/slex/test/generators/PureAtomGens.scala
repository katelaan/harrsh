package slex.slex.test.generators

import org.scalacheck.Gen
import slex.seplog.{False, IntExpr, IxEq, IxGEq, IxGT, IxLEq, IxLT, IxNEq, PtrEq, PtrExpr, PtrNEq, PureAtom, True}

/**
  * Created by jens on 10/9/16.
  */
object PureAtomGens {

  /**
    * Generator for pure atoms containng the given sets of variables and constant values
    */
  def pureAtomGen(vars : Set[String], vals : Set[Int]) : Gen[PureAtom] = Gen.frequency(
    (1, Gen.const(True())),
    (1, Gen.const(False())),
    (6, atomsWithIntsGen(vars, vals)),
    (4, atomsWithPtrsGen(vars))
  )

  private def atomsWithIntsGen(vars : Set[String], vals : Set[Int]) : Gen[PureAtom] =
    for {
      l <- SlExprGens.intExprGen(vars, vals)
      r <- SlExprGens.intExprGen(vars, vals)
      a <- inject(l,r)
    } yield a

  private def atomsWithPtrsGen(vars : Set[String]) : Gen[PureAtom] =
    for {
      l <- SlExprGens.ptrExprGen(vars)
      r <- SlExprGens.ptrExprGen(vars)
      a <- inject(l,r)
    } yield a

  private def inject(l : IntExpr, r : IntExpr) : Gen[PureAtom] = Gen.oneOf(
    IxEq(l, r),
    IxGT(l, r),
    IxLT(l, r),
    IxLEq(l, r),
    IxGEq(l, r),
    IxNEq(l, r)
  )

  private def inject(l : PtrExpr, r : PtrExpr) : Gen[PureAtom] = Gen.oneOf(
    PtrEq(l, r),
    PtrNEq(l, r)
  )

}
