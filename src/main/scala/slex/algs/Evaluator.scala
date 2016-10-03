package slex.algs

import slex.slsyntax.{IntNEq, IxEq, IxGEq, IxGT, IxLEq, IxLT, PtrEq, PtrNEq, PureAnd, PureAtom, PureFormula, PureNeg, PureOr, True}

/**
  * Created by jkatelaa on 10/3/16.
  */
object Evaluator {

  def eval(s : Stack, phi : PureFormula) : Boolean = phi match {
    case PureNeg(phi) => !eval(s, phi)
    case PureAnd(phi, psi) => eval(s, phi) && eval(s, psi)
    case PureOr(phi, psi) => eval(s, phi) || eval(s, psi)
    case a : PureAtom => a match {
        // FIXME Evaluation of pure atoms
      case True() => true
      case IxEq(l, r) => ???
      case IxGT(l, r) => ???
      case IxLT(l, r) => ???
      case IxLEq(l, r) => ???
      case IxGEq(l, r) => ???
      case IntNEq(l, r) => ???
      case PtrEq(l, r) => ???
      case PtrNEq(l, r) => ???
    }
    case _ => ???
  }

}
