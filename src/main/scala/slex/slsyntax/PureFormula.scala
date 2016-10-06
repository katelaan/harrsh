package slex.slsyntax

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

  def foldConstants : PureFormula
}

case class PureNeg(phi : PureFormula) extends PureFormula {

  override def toString = "(\u00ac " + phi + ")"

  override def isSymbolicHeap: Boolean = false

  override def toSymbolicHeap: Option[SymbolicHeap] = None

  override def toSmtExpr: SmtExpr = notExpr(phi.toSmtExpr)

  override def constantEval: Option[Boolean] = phi.constantEval map (b => !b)

  override def foldConstants: PureFormula = {
    logger.debug("Folding constants of " + this)
    phi.constantEval match {
      case Some(b) => if (b) False() else True()
      case None =>
        val res = PureNeg(phi.foldConstants)
        logger.debug("Simplifying argument of " + this + ", yielding " + res)
        res
    }
  }
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

  override def foldConstants: PureFormula = {
    logger.debug("Folding constants of " + this)
    (phi.constantEval, psi.constantEval) match {
      case (Some(true), _) =>
        val res = psi.foldConstants
        logger.debug("Discarding first argument, yielding " + res)
        res
      case (Some(false), _) =>
        logger.debug("Yielding false")
        False()
      case (_, Some(true)) =>
        val res = phi.foldConstants
        logger.debug("Discarding second argument, yielding " + res)
        res
      case (_, Some(false)) =>
        logger.debug("Yielding false")
        False()
      case (None, None) =>
        val res = PureAnd(phi.foldConstants, psi.foldConstants)
        logger.debug("Simplifying arguments of " + this + ", yielding " + res)
        res
    }
  }
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

  override def foldConstants: PureFormula = {
    logger.debug("Folding constants of " + this)
    (phi.constantEval, psi.constantEval) match {
      case (Some(false), _) =>
        val res = psi.foldConstants
        logger.debug("Discarding first argument, yielding " + res)
        res
      case (Some(true), _) =>
        logger.debug("Yielding true")
        True()
      case (_, Some(false)) =>
        val res = phi.foldConstants
        logger.debug("Discarding second argument, yielding " + res)
        res
      case (_, Some(true)) =>
        logger.debug("Yielding true")
        True()
      case (None, None) =>
        val res = PureOr(phi.foldConstants, psi.foldConstants)
        logger.debug("Simplifying arguments of " + this + ", yielding " + res)
        res
    }
  }
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
      case IntNEq(l, r) => l.collectIdents ++ r.collectIdents
      case PtrEq(l, r) => Set() ++ l.getIdent ++ r.getIdent
      case PtrNEq(l, r) => Set() ++ l.getIdent ++ r.getIdent
    }
  }

}
