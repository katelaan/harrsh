package at.forsyte.harrsh.converters

import scala.language.implicitConversions

import at.forsyte.harrsh.main.ProblemStatus.{Correct, Incorrect, Unknown}
import at.forsyte.harrsh.main.{EntailmentQuery, ProblemStatus}
import at.forsyte.harrsh.parsers.QueryParser.FileExtensions
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}
import at.forsyte.harrsh.seplog.inductive._

object ToSlcompConverter extends EntailmentFormatConverter {

  val Logic = "QF_SHID" // TODO: Detect when we're in one of the other fragments?

  override def apply(fileName: String, query: EntailmentQuery): Seq[(String, String)] = {
    Seq((fileName + FileExtensions.SlComp, queryToSlcomp(query)))
  }

  private sealed trait SExpr {
    def prettyPrint: String = prettyPrintLines.mkString("\n")
    def prettyPrintLines: Seq[String]
  }

  private case class App(args: SExpr*) extends SExpr {
    assert(args.nonEmpty)

    override def prettyPrintLines: Seq[String] = {
      val ppArgs: Seq[String] = args.flatMap(_.prettyPrintLines)

      if (ppArgs.map(_.length).sum < 80)
        Seq(s"(${ppArgs.mkString(" ")})")
      else {
        if (ppArgs.size <= 1) {
          Seq(ppArgs.mkString("(", "", ")"))
        }
        else {
          val hd = ppArgs.head
          val mid = ppArgs.tail.init
          val last = ppArgs.last
          Seq("(" + hd) ++ mid.map("  "+_) ++ Seq("  " + last +")")
        }
      }
    }
  }

  private case class GroundTerm(s: String) extends SExpr {

    override def prettyPrintLines: Seq[String] = Seq(s)
  }

  private implicit def strToGroundTerm(s: String) : GroundTerm = GroundTerm(s)

  private def queryToSlcomp(query: EntailmentQuery): String = {
    val EntailmentQuery(lhs, rhs, sid, status, _) = query

    val ptoArities = pointerArities(query.sid, query.lhs, query.rhs)
    if (ptoArities.size != 1)
      throw ConversionException("Multiple pointer arities within one benchmark not supported by Slcomp exporter: " + ptoArities)
    val arity = ptoArities.head

    val script: Seq[SExpr] = Seq(
      // set-logic
      Seq(logic),
      // set-info
      statusToMeta(status).toSeq,
      // declare-sort
      sortDecls(arity),
      // declare-datatypes
      Seq(datatypeDecls(arity)),
      // declare heap
      Seq(heapDecl(arity)),
      // define-funs-rec
      Seq(funDefs(arity, sid)),
      // variables
      varDecls(arity, lhs, rhs),
      // asserts
      Seq(shToAssert(arity, lhs, negate = false), shToAssert(arity, rhs, negate = true)),
      // check sat
      Seq(checkSat)
    ).flatten

    script.flatMap(_.prettyPrintLines).mkString("\n")
  }

  private lazy val logic = App("set-logic", Logic)

  private lazy val checkSat = App("check-sat")

  private def sortDecls(arity: Int): Seq[SExpr] = Seq(
    App("declare-sort", sort(arity), "0")
  )

  private def datatypeDecls(arity: Int): SExpr = App("declare-datatypes",
    // (Singleton) List of types
    App(App(dtype(arity), "0")),
    // (Singleton) List of constructor lists
    App(makeConstructors(arity))
  )

  private def makeConstructors(arity: Int): SExpr = {
    // Singleton constructor list
    App(
      App(GroundTerm(constructor(arity)) +: fields(arity) :_*)
    )
  }

  private def heapDecl(arity: Int): SExpr = {
    App("declare-heap", App(sort(arity), dtype(arity)))
  }

  private def statusToMeta(status: ProblemStatus): Option[SExpr] = {
    val statusString = status match {
      case Correct => Some("unsat")
      case Incorrect => Some("sat")
      case Unknown => None
    }
    statusString map (App("set-info", ":status",_))
  }

  private def funDefs(arity: Int, sid: SID): SExpr = App(
    "define-funs-rec", funDecls(arity, sid), funBodies(arity, sid)
  )

  private def funDecls(arity: Int, sid: SID): SExpr = App(
    sid.preds.map(funDecl(arity, _)) : _*
  )

  private def funBodies(arity: Int, sid: SID): SExpr = App(
    sid.preds.map(funBody(arity, _)) : _*
  )

  private def funDecl(arity: Int, pred: Predicate): SExpr = App(
    pred.head, App(pred.params.map(v => App(v.name, sort(arity))) : _*), "Bool"
  )

  private def funBody(arity: Int, pred: Predicate): SExpr = {
    val ruleExprs = pred.rules map (ruleExpr(arity, _))
    if (ruleExprs.size == 1)
      ruleExprs.head
    else
      App(GroundTerm("or") +: ruleExprs : _*)
  }

  private def ruleExpr(arity: Int, body: RuleBody): SExpr = shToSExpr(arity, body.body, Some(body.qvarNames))

  private def varDecls(arity: Int, shs: SymbolicHeap*): Seq[SExpr] = {
    val vars = for {
      sh <- shs.toSet[SymbolicHeap]
      fv <- sh.freeVars
    } yield fv
    vars.map(fv => App("declare-const", fv.name, sort(arity))).toSeq
  }

  private def shToAssert(arity: Int, sh: SymbolicHeap, negate: Boolean): SExpr = {
    // Currently we're only translating quantifier-free formulas
    // The translation could in principle also work for quantified formulas,
    // but would then currently be irreversible because bound var names would be lost
    assert(sh.boundVars.isEmpty)
    val formula = shToSExpr(arity, sh, None)
    val signed = if (negate) App("not", formula) else formula
    App("assert", signed)
  }

  private def shToSExpr(arity: Int, sh: SymbolicHeap, boundVarNames: Option[Seq[String]]): SExpr = {
    val pto = sh.pointers map (ptoToExpr(arity, boundVarNames, _))
    val calls = sh.predCalls map (callToExpr(arity, boundVarNames, _))
    val spatialAtoms = pto ++ calls
    val spatial = if (spatialAtoms.isEmpty) {
      App("_", "emp", sort(arity), dtype(arity))
    } else {
      App(GroundTerm("sep") +: spatialAtoms : _*)
    }
    val pure = sh.pure map (pureAtomToExpr(arity, boundVarNames, _))
    val qfree = if (pure.nonEmpty) {
      App(GroundTerm("and") +: pure :+ spatial : _*)
    } else {
      spatial
    }
    if (sh.boundVars.isEmpty) {
      qfree
    } else {
      val quantifiers = App(sh.boundVars.toSeq.map(v => App(v2e(arity, v, boundVarNames), sort(arity))) : _*)
      App("exists", quantifiers, qfree)
    }
  }

  private def ptoToExpr(arity: Int, boundVarNames: Option[Seq[String]], pto: PointsTo): SExpr = {
    val rhs = App(GroundTerm(constructor(arity)) +: pto.to.map(v2e(arity, _, boundVarNames)) : _*)
    App("pto", v2e(arity, pto.from, boundVarNames), rhs)
  }

  private def callToExpr(arity: Int, boundVarNames: Option[Seq[String]], call: PredCall): SExpr = App(GroundTerm(call.name ) +: call.args.map(v2e(arity, _, boundVarNames)) : _*)

  private def pureAtomToExpr(arity: Int, boundVarNames: Option[Seq[String]], pure: PureAtom): SExpr = {
    val op = if (pure.isEquality) "=" else "distinct"
    App(op, v2e(arity, pure.l, boundVarNames), v2e(arity, pure.r, boundVarNames))
  }

  private def v2e(arity: Int, v: Var, boundVarNames: Option[Seq[String]] = None): SExpr = v match {
    case FreeVar(name) => GroundTerm(name)
    case NullConst => App("as", "nil", sort(arity))
    case BoundVar(index) => boundVarNames match {
      case None => GroundTerm("_" + index)
      case Some(names) => GroundTerm(names(index - 1))
    }
  }

  private def field(idx: Int): String = "f" + idx

  private def fields(arity: Int): Seq[SExpr] = (1 to arity) map (i => App(GroundTerm(field(i)), sort(arity)))

  private def sort(arity: Int): String = "Refnode" + arity

  private def dtype(arity: Int): String = "node" + arity

  private def constructor(arity: Int): String = "c_node" + arity

}
