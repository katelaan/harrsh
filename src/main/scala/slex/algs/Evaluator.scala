package slex.algs

import slex.slsyntax._

/**
  * Created by jkatelaa on 10/3/16.
  */
object Evaluator {

  def eval(s: Stack, phi: PureFormula): Boolean = phi match {
    case PureNeg(phi) => !eval(s, phi)
    case PureAnd(phi, psi) => eval(s, phi) && eval(s, psi)
    case PureOr(phi, psi) => eval(s, phi) || eval(s, psi)
    case a: PureAtom => a match {
      case True() => true
      case False() => false
      case IxEq(l, r) => eval(s, l) == eval(s, r)
      case IxGT(l, r) => eval(s, l) > eval(s, r)
      case IxLT(l, r) => eval(s, l) < eval(s, r)
      case IxLEq(l, r) => eval(s, l) <= eval(s, r)
      case IxGEq(l, r) => eval(s, l) >= eval(s, r)
      case IntNEq(l, r) => eval(s, l) != eval(s, r)
      case PtrEq(l, r) => eval(s, l) == eval(s, r)
      case PtrNEq(l, r) => eval(s, l) != eval(s, r)
    }
    case _ => ???
  }

  def eval(s: Stack, ix: IntExpr): Int = ix match {
    case IntConst(n) => n
    case v: IntVar => s(v)
    case Plus(l, r) => eval(s, l) + eval(s, r)
    case Minus(l, r) => eval(s, l) - eval(s, r)
  }

  def eval(s: Stack, p: PtrExpr): Int = s(p) // FIXME [NULL-TREATMENT] The value of evaluating null might change in the future...

}