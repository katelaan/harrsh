package at.forsyte.harrsh.parsers.slcomp

import at.forsyte.harrsh.converters.ToSlcompConverter
import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.seplog._

import scala.collection.mutable.ListBuffer

object ScriptToQuery extends HarrshLogging {

  def DEFAULT_SELECTOR = "_def"

  def apply(s: Script, fileName: String): Query = {
    if (s.asserts.isEmpty || s.asserts.length > 2) {
      throw new Exception(s"Can only deal with queries with 1 or 2 asserts, but received ${s.asserts.length}")
    }

    logger.debug(s"Will translate the following script:\n$s")
    //println(s"Will translate the following script:\n$s")

    val sorts: Set[Sort] = s.sorts.map(decl => Sort(decl.name)).toSet

    // Harrsh doesn't support named selectors, so we'll instead create a map from selector names to positions on the right-hand side of a fixed-width right-hand side
    val types: DataTypes = s.types.getOrElse(DataTypes(Nil))
    val sels: List[String] = extractSelectors(s.heap, types)
    logger.debug(s"Selectors occurring in heap: $sels")
    val selToIx: Map[String, Int] = sels.zipWithIndex.toMap
    logger.debug(s"Asscoiated with indices: $selToIx")

    val consts = s.consts.map(_.name.str)
    val constsToFvs: Map[String, Var] = Map() ++ consts.map(c => (c, FreeVar(c)))
    logger.debug(s"Mapping consts to FVs: $constsToFvs")

    val preds: Set[String] = s.funs.map(_.decl.name.str).toSet

    val env = Env(preds, types, selToIx)

    val assertToSh = (a: Assert) => collectAtoms(a.term, env, constsToFvs).head.toSymbolicHeap

    val left = assertToSh(s.asserts.head)
    val maybeRight = s.asserts.tail.headOption map {
      a =>
        // The second assert must be negated (and hence correspond to the RHS of the entailment)
        assert(a.term match {
        case Args(Symbol("not") :: args) => true
        case _ => false
        })

        assertToSh(a)
    }

    val rules: Seq[(String,RuleBody)] = s.funs flatMap (fun => funDefToRules(fun, env))

    logger.debug(s"Top-level assertion(s):\n${(List(left) ++ maybeRight).mkString("\n")}")
    logger.debug(s"Predicate definitions:\n${rules.mkString("\n")}")

    // FIXME: Compute rootedness?
    val sid = SidFactory.makeSidfromRuleBodies("undefined", rules, fileName)
    val status = s.status.getOrElse(ProblemStatus.Unknown)

    maybeRight match {
      case Some(right) =>
        // In SLCOMP format, UNSAT means that the entailment holds and SAT that it does not hold
        // We therefore flip the status for our internal representation
        val res = EntailmentQuery(left, right, sid, status.flip, Some(fileName))
        //println(s"In SMT format:\n${ToSlcompConverter(fileName, res)}")
        res
      case None =>
        SatQuery(sid, left, status, Some(fileName))
    }
  }

  case class Env(preds: Set[String], types: DataTypes, selToIx: Map[String, Int]) {

    def mkPtrTrg(sels: List[(Var,String)]): Seq[Var] = {
      val trg: Array[Var] = Array.fill(selToIx.size)(NullConst)
      for {
        (ptr,sel) <- sels
      } trg(selToIx(sel)) = ptr
      trg.toList
    }
  }

  def funDefToRules(fun: FunDef, env: Env): Seq[(String, RuleBody)] = {
    val head = fun.decl.name.str
    val freeVars = fun.decl.args.map(_.name.str) map FreeVar
    val varMap = (for {
      (arg, i) <- fun.decl.args.zipWithIndex
      argStr = arg.name.str
    } yield (argStr, FreeVar(argStr))).toMap
    val atoms: Seq[Atoms] = collectAtoms(fun.term, env, varMap)
    for {
      atom <- atoms
    } yield (head, RuleBody(atom.qvars, atom.toSymbolicHeap.copy(freeVars = freeVars)))
  }

  case class Atoms(pure: List[PureAtom], pointsTo: List[PointsTo], predCalls: List[PredCall], qvars: List[String]) {
    def toSymbolicHeap: SymbolicHeap = {
      val atoms = AtomContainer(pure, pointsTo, predCalls)
      logger.debug(s"Creating SH from $atoms with free vars ${atoms.freeVarSeq}, bound vars: $qvars")
      SymbolicHeap(atoms, atoms.freeVarSeq)
    }

    def merge(other: Atoms) : Atoms = {
      if (qvars.nonEmpty && other.qvars.nonEmpty) throw new Exception(s"Can't merge two terms that contain bound vars: $this / $other")
      Atoms(pure ++ other.pure, pointsTo ++ other.pointsTo, predCalls ++ other.predCalls, qvars ++ other.qvars)
    }
  }

  object Atoms {
    def apply(pureAtom: PureAtom): Atoms = Atoms(List(pureAtom), Nil, Nil, Nil)
    def apply(pointsTo: PointsTo): Atoms = Atoms(Nil, List(pointsTo), Nil, Nil)
    def apply(predCall: PredCall): Atoms = Atoms(Nil, Nil, List(predCall), Nil)

    def mergeAll(atoms: Seq[Atoms]): Atoms = atoms match {
      case last +: Seq() => last
      case head +: tail => head.merge(mergeAll(tail))
    }
  }

  def collectAtoms(term: SidBuilder, env: Env, varMap: Map[String,Var]): List[Atoms] = term match {
    case Args(Symbol(fn) :: args) =>
      fn match {
        case "or" =>
          args flatMap (arg => collectAtoms(arg, env, varMap))
        case "pto" =>
          assert(args.length == 2)
          logger.debug(s"Ptr from src ${args.head} to targets ${args(1)}")
          val src = qualIdentToVar(args.head, varMap)
          val trgs = constructorToVars(args(1), env, varMap)
          val pto = PointsTo(src, trgs)
          List(Atoms(pto))
        case "and" =>
          logger.debug(s"Applying and to ${args.length} args $args")
          // TODO: Reduce code duplication w.r.t. sep
          val argAtomss = args map (arg => collectAtoms(arg, env, varMap))
          assert(argAtomss forall (_.length == 1))
          List(Atoms.mergeAll(argAtomss map (_.head)))
        case "sep" =>
          logger.debug(s"Applying sep to ${args.length} args $args")
          for (arg <- args) logger.debug (s" - $arg")
          val argAtomss = args map (arg => collectAtoms(arg, env, varMap))
          assert(argAtomss forall (_.length == 1))
          List(Atoms.mergeAll(argAtomss map (_.head)))
        case "wand" =>
          throw new Exception("No support for the magic wand")
        case "distinct" =>
          //val vars = args map (_.asInstanceOf[Symbol].str)
          val neqs = for {
            (left, i) <- args.zipWithIndex
            (right, j) <- args.zipWithIndex
            if i < j
          } yield qualIdentToVar(left, varMap) =/= qualIdentToVar(right, varMap)
          List(Atoms(neqs, Nil, Nil, Nil))
        //case "emp" =>
        //  ???
        case "=" =>
          logger.debug(s"Applying = to $args")
          assert(args.length == 2)
          val ops = args map (arg => qualIdentToVar(arg, varMap))
          List(Atoms(ops(0) =:= ops(1)))
        case "not" =>
          // Simply ignore not (can only appear as outermost operator of second assertion)
          assert(args.length == 1)
          collectAtoms(args.head, env, varMap)
        case pred if env.preds.contains(pred) =>
          val callArgs = args map (arg => qualIdentToVar(arg, varMap))
          List(Atoms(PredCall(pred, callArgs)))
        case other =>
          throw new Exception(s"Can't convert $other to symbolic heap")
      }
    case IndexedIdentifier(Symbol("emp"),_) =>
      List(Atoms(Nil, Nil, Nil, Nil))
    case Exists(vars, term) =>
      val qvars: List[String] = vars map (_.name.str)
      val qvarMap = qvars.zipWithIndex.map {
        case (str,ix) => (str,BoundVar(ix+1))
      }
      val extendedMap = varMap ++ qvarMap
      val termAtoms = collectAtoms(term, env, extendedMap)
      assert(termAtoms.length == 1)
      List(termAtoms.head.copy(qvars = qvars))
    case other =>
      throw new Exception(s"Can't convert $other to symbolic heap")
  }

  def constructorToVars(sid : SidBuilder, env: Env, varMap: Map[String,Var]): Seq[Var] = sid match {
    case Args((s@Symbol(hd)) :: tl) =>
      if (tl.isEmpty) {
        env.mkPtrTrg(List((qualIdentToVar(s, varMap), DEFAULT_SELECTOR)))
      } else {
        val args = tl map (arg => qualIdentToVar(arg, varMap))
        val c = env.types.getConstructor(hd)
        val sels = c.sels.map(_.sel.str)
        env.mkPtrTrg(args zip sels)
      }
    case other =>
      throw new Exception(s"Can't convert $other to constructor application")
  }

  def qualIdentToVar(sid : SidBuilder, varMap: Map[String,Var]): Var = sid match {
    case Symbol(str) => varMap(str)
    case QualifiedIdentifier(Symbol(str), _) =>
      // We don't care about the type info
      if (str == "nil") NullConst else throw new Exception(s"Unexpected qualified identifier $str")
    case other =>
      throw new Exception(s"Can't interpret $sid as qualified identifier")
  }

  def extractSelectors(heapDecl: HeapDecl, types: DataTypes): List[String] = {
    val res = ListBuffer.empty[String]

    for {
      (src, trg) <- heapDecl.mapping
    } {
      val maybeDt = types.get(trg.symbol.str)
      maybeDt match {
        case None =>
          // The target is a built-in type => anonymous selector field
          res += DEFAULT_SELECTOR
        case Some(dt) =>
          for {
            c <- dt.constructors
            selDecl <- c.sels
          } {
            res += selDecl.sel.str
          }
      }
    }

    res.toList.distinct
  }

}
