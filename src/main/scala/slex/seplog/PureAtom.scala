package slex.seplog

import slex.Sorts.Location
import slex.models.{StackBasedEvaluator, Stack}
import slex.main.SlexLogging
import slex.smtsyntax.SmtExpr
import slex.smtsyntax.SmtExpr._

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait PureAtom extends SepLogFormula with PureFormula with SlexLogging {

  override def isSpatial = false

  override def isPure = true

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this), Seq(), Seq()))

  def negate : PureAtom = this match {
    case True() => False()
    case False() => True()

    case IxEq(l, r) => IxNEq(l, r)
    case IxNEq(l, r) => IxEq(l, r)

    case IxGT(l, r) => IxLEq(l, r)
    case IxLEq(l, r) => IxGT(l, r)

    case IxLT(l, r) => IxGEq(l, r)
    case IxGEq(l, r) => IxLT(l, r)

    case PtrEq(l, r) => PtrNEq(l, r)
    case PtrNEq(l, r) => PtrEq(l, r)
  }

  private def syntaxBasedConstantEval : Option[Boolean] = {
    def foldIfSyntacticallyEqual(res : => Boolean, l : Expr, r : Expr) : Option[Boolean] = if (l == r) Some(res) else None

    this match {
      case True() => None
      case False() => None
      case IxEq(l, r) => foldIfSyntacticallyEqual(true, l, r)
      case IxGT(l, r) => foldIfSyntacticallyEqual(false, l, r)
      case IxLT(l, r) => foldIfSyntacticallyEqual(false, l, r)
      case IxLEq(l, r) => foldIfSyntacticallyEqual(true, l, r)
      case IxGEq(l, r) => foldIfSyntacticallyEqual(true, l, r)
      case IxNEq(l, r) => foldIfSyntacticallyEqual(false, l, r)
      case PtrEq(l, r) => foldIfSyntacticallyEqual(true, l, r)
      case PtrNEq(l, r) => foldIfSyntacticallyEqual(false, l, r)
    }
  }

  override def constantEval: Option[Boolean] = {
    try {
      // Try to evaluate on constant stack. If that's possible, we have a constant value...
      val res = StackBasedEvaluator.eval(dummyStack, this)
      Some(res)
    } catch {
      case _ : DummyException =>
        // ...otherwise we also have a constant value if the two arguments of a binary atom are synactically equal
        // TODO Of course richer rewriting rules than just checking for equality would also be possible here. Is that worth exploring?
        val res = syntaxBasedConstantEval
        res
    }
  }

  override def renameVars(f: Renaming): PureAtom = this match {
    case t : True => t
    case f : False => f
    case IxEq(l, r) => IxEq(l.renameVars(f), r.renameVars(f))
    case IxGT(l, r) => IxGT(l.renameVars(f), r.renameVars(f))
    case IxLT(l, r) => IxLT(l.renameVars(f), r.renameVars(f))
    case IxLEq(l, r) => IxLEq(l.renameVars(f), r.renameVars(f))
    case IxGEq(l, r) => IxGEq(l.renameVars(f), r.renameVars(f))
    case IxNEq(l, r) => IxNEq(l.renameVars(f), r.renameVars(f))
    case PtrEq(l, r) => PtrEq(l.renameVars(f), r.renameVars(f))
    case PtrNEq(l, r) => PtrNEq(l.renameVars(f), r.renameVars(f))
  }

  def getVars : Set[String] = this match {
    case True() => Set()
    case False() => Set()
    case IxEq(l, r) => l.getVars union r.getVars
    case IxGT(l, r) => l.getVars union r.getVars
    case IxLT(l, r) => l.getVars union r.getVars
    case IxLEq(l, r) => l.getVars union r.getVars
    case IxGEq(l, r) => l.getVars union r.getVars
    case IxNEq(l, r) => l.getVars union r.getVars
    case PtrEq(l, r) => l.getVar union r.getVar // TODO Building so many sets is quite inefficient
    case PtrNEq(l, r) => l.getVar union r.getVar
  }

  def simplify : PureFormula = {
    logger.debug("Trying to eval " + this + " to a constant yielding " + constantEval)
    PureAtom.replaceByConstIfDefined(this, constantEval)
  }

  private case class DummyException() extends Throwable

  private lazy val dummyStack = new Stack() {
    override def apply(ptr: PtrExpr): Location = throw new DummyException
    override def apply(ix: IntExpr): Int  = throw new DummyException
  }

}

case class True() extends PureAtom {
  override def toString = "true"

  override def toSmtExpr: SmtExpr = "true"
}

case class False() extends PureAtom {
  override def toString = "false"

  override def toSmtExpr: SmtExpr = "false"
}

case class IxEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2248 " + r

  override def toSmtExpr: SmtExpr = eqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxGT(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " > " + r

  override def toSmtExpr: SmtExpr = gtExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxLT(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " < " + r

  override def toSmtExpr: SmtExpr = ltExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxLEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2264 " + r

  override def toSmtExpr: SmtExpr = leqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxGEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2265 " + r

  override def toSmtExpr: SmtExpr = geqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxNEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2249 " + r

  override def toSmtExpr: SmtExpr = neqExpr(l.toSmtExpr, r.toSmtExpr)

}

case class PtrEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toString = l + " \u2248 " + r

  override def toSmtExpr: SmtExpr = eqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class PtrNEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toString = l + " \u2249 " + r

  override def toSmtExpr: SmtExpr = neqExpr(l.toSmtExpr, r.toSmtExpr)
}

object PureAtom {

  def replaceByConstIfDefined(pf : PureFormula, const : Option[Boolean]) : PureFormula = const match {
    case Some(true) => True()
    case Some(false) => False()
    case None => pf
  }

}