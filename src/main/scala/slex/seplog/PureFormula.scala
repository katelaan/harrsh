package slex.seplog

import slex.main.SlexLogging
import slex.smtsyntax.SmtExpr
import slex.smtsyntax.SmtExpr._

/**
  * Created by jkatelaa on 10/3/16.
  */
trait PureFormula extends SepLogFormula with SlexLogging {

  override def isPure = true

  override def isSpatial = false

  def toSmtExpr : SmtExpr

  def constantEval : Option[Boolean]

  override def renameVars(f: String => String): PureFormula

  /**
    * Simplify the formula
    * @return Formula where constant subformulas have been evaluated and negations pushed into atoms, but without more aggressive simplifications
    */
  def simplify : PureFormula
}

case class PureNeg(phi : PureFormula) extends PureFormula {

  override def toString = "(\u00ac " + phi + ")"

  override def isSymbolicHeap: Boolean = false

  override def toSymbolicHeap: Option[SymbolicHeap] = None

  override def toSmtExpr: SmtExpr = notExpr(phi.toSmtExpr)

  override def constantEval: Option[Boolean] = phi.constantEval map (b => !b)

  override def simplify: PureFormula = {
    logger.debug("Folding constants of " + this)
    phi.constantEval match {
      case Some(b) => if (b) False() else True()
      case None =>
        val simplifiedArg = phi.simplify

        val res = if (simplifiedArg.isInstanceOf[PureAtom]) {
          simplifiedArg.asInstanceOf[PureAtom].negate
        }
        else {
          PureNeg(simplifiedArg)
        }

        logger.debug("Simplifying argument of " + this + ", yielding " + res)

        res
    }
  }

  override def renameVars(f: String => String): PureFormula = PureNeg(phi.renameVars(f))
}

case class PureAnd(phi : PureFormula, psi : PureFormula) extends PureFormula {

  override def toString = "(" + phi + " \u2227 " + psi + ")"

  override def isSymbolicHeap: Boolean = phi.isSymbolicHeap && psi.isSymbolicHeap

  override def toSymbolicHeap: Option[SymbolicHeap] = SymbolicHeap.combineHeaps(phi.toSymbolicHeap, psi.toSymbolicHeap)

  override def toSmtExpr: SmtExpr = andExpr(phi.toSmtExpr, psi.toSmtExpr)

  override def constantEval: Option[Boolean] = {
    (phi.constantEval, psi.constantEval) match {
      case (Some(bl), Some(br)) => Some(bl && br)
      case (Some(false), _) => Some(false)
      case (_, Some(false)) => Some(false)
      case _ => None
    }
  }

  override def simplify: PureFormula = {
    logger.debug("Folding constants of " + this)
    (phi.constantEval, psi.constantEval) match {
      case (Some(true), _) =>
        val res = psi.simplify
        logger.debug("Discarding first argument, yielding " + res)
        res
      case (Some(false), _) =>
        logger.debug("Yielding false")
        False()
      case (_, Some(true)) =>
        val res = phi.simplify
        logger.debug("Discarding second argument, yielding " + res)
        res
      case (_, Some(false)) =>
        logger.debug("Yielding false")
        False()
      case (None, None) =>
        val res = PureAnd(phi.simplify, psi.simplify)
        logger.debug("Simplifying arguments of " + this + ", yielding " + res)
        res
    }
  }

  override def renameVars(f: String => String): PureFormula = PureAnd(phi.renameVars(f), psi.renameVars(f))
}

//case class PureImplies(phi : PureFormula, psi : PureFormula) extends PureFormula {
//
//  override def toString = "(" + phi + " => " + psi + ")"
//
//  override def isSymbolicHeap: Boolean = false
//
//  override def toSymbolicHeap: Option[SymbolicHeap] = None
//
//  override def toSmtExpr: SmtExpr = impliesExpr(phi.toSmtExpr, psi.toSmtExpr)
//}

case class PureOr(phi : PureFormula, psi : PureFormula) extends PureFormula {

  override def toString = "(" + phi + " \u2228 " + psi + ")"

  override def isSymbolicHeap: Boolean = false

  override def toSymbolicHeap: Option[SymbolicHeap] = None

  override def toSmtExpr: SmtExpr = orExpr(phi.toSmtExpr, psi.toSmtExpr)

  override def constantEval: Option[Boolean] = {
    (phi.constantEval, psi.constantEval) match {
      case (Some(bl), Some(br)) => Some(bl || br)
      case (Some(true), _) => Some(true)
      case (_, Some(true)) => Some(true)
      case _ => None
    }
  }

  override def simplify: PureFormula = {
    logger.debug("Folding constants of " + this)
    (phi.constantEval, psi.constantEval) match {
      case (Some(false), _) =>
        val res = psi.simplify
        logger.debug("Discarding first argument, yielding " + res)
        res
      case (Some(true), _) =>
        logger.debug("Yielding true")
        True()
      case (_, Some(false)) =>
        val res = phi.simplify
        logger.debug("Discarding second argument, yielding " + res)
        res
      case (_, Some(true)) =>
        logger.debug("Yielding true")
        True()
      case (None, None) =>
        val res = PureOr(phi.simplify, psi.simplify)
        logger.debug("Simplifying arguments of " + this + ", yielding " + res)
        res
    }
  }

  override def renameVars(f: String => String): PureFormula = PureOr(phi.renameVars(f), psi.renameVars(f))
}

object PureFormula {

  def collectIdentifiers(phi : PureFormula) : Set[String] = phi match {
    case PureNeg(phi) => collectIdentifiers(phi)
    case PureAnd(phi, psi) => collectIdentifiers(phi) union collectIdentifiers(psi)
    //case PureImplies(phi, psi) => collectIdentifiers(phi) union collectIdentifiers(psi)
    case PureOr(phi, psi) => collectIdentifiers(phi) union collectIdentifiers(psi)
    case a : PureAtom => a match {
      case True() => Set()
      case False() => Set()
      case IxEq(l, r) => l.collectIdents ++ r.collectIdents
      case IxGT(l, r) => l.collectIdents ++ r.collectIdents
      case IxLT(l, r) => l.collectIdents ++ r.collectIdents
      case IxLEq(l, r) => l.collectIdents ++ r.collectIdents
      case IxGEq(l, r) => l.collectIdents ++ r.collectIdents
      case IxNEq(l, r) => l.collectIdents ++ r.collectIdents
      case PtrEq(l, r) => Set() ++ l.getIdent ++ r.getIdent
      case PtrNEq(l, r) => Set() ++ l.getIdent ++ r.getIdent
    }
  }

}
