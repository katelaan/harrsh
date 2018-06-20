package at.forsyte.harrsh.parsers.slcomp

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._

import scala.collection.mutable.ListBuffer

object ScriptToSatBenchmark extends HarrshLogging {

  def DefaultSel = "_def"

  def apply(s: Script, description: String): SatBenchmark = {
    if (s.asserts.length != 1) {
      throw new Exception(s"Can only build top-level symbolic heap from 1 assert, but received ${s.asserts.length}")
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
    val constsToFvs: Map[String, Var] = (for {
      (c, i) <- consts.zipWithIndex
    } yield (c, Var(i+1))).toMap
    logger.debug(s"Mapping consts to FVs: $constsToFvs")

    val preds: Set[String] = s.funs.map(_.decl.name.str).toSet

    val env = Env(preds, types, selToIx)

    val sh = collectAtoms(s.asserts.head.term, env, constsToFvs).head.toSymbolicHeap
    val rules: Seq[Rule] = s.funs flatMap (fun => funDefToRules(fun, env))

    logger.debug(s"Top-level assertion: $sh")
    logger.debug(s"Predicate definitions:\n${rules.mkString("\n")}")

    // TODO: Avoid blowing up numFV to consts.length by dedicated support for top-level queries
    val numFV = rules.map(_.body.numFV).max
    val sid = SID("undefined", rules, description, numFV)
    val status = s.status.getOrElse(SatBenchmark.Unknown)
    SatBenchmark(sid, consts, sh, status)
  }

  case class Env(preds: Set[String], types: DataTypes, selToIx: Map[String, Int]) {

    def mkPtrTrg(sels: List[(PtrExpr,String)]): Seq[PtrExpr] = {
      val trg: Array[PtrExpr] = Array.fill(selToIx.size)(NullPtr())
      for {
        (ptr,sel) <- sels
      } trg(selToIx(sel)) = ptr
      trg.toList
    }
  }

  def funDefToRules(fun: FunDef, env: Env): Seq[Rule] = {
    val head = fun.decl.name.str
    val freeVars = fun.decl.args.map(_.name.str)
    val varMap = (for {
      (arg, i) <- fun.decl.args.zipWithIndex
      argStr = arg.name.str
    } yield (argStr, Var(i+1))).toMap
    val atoms: Seq[Atoms] = collectAtoms(fun.term, env, varMap)
    for {
      atom <- atoms
    } yield Rule(head, freeVars, atom.qvars, atom.toSymbolicHeap)
  }

  case class Atoms(pure: List[PureAtom], pointsTo: List[PointsTo], predCalls: List[PredCall], qvars: List[String]) {
    def toSymbolicHeap: SymbolicHeap = {
      SymbolicHeap(pure, pointsTo, predCalls)
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
          val src = qualIdentToPtrExpr(args.head, varMap)
          val trgs = constructorToPtrExprs(args(1), env, varMap)
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
          } yield PtrNEq(qualIdentToPtrExpr(left, varMap), qualIdentToPtrExpr(right, varMap))
          List(Atoms(neqs, Nil, Nil, Nil))
        //case "emp" =>
        //  ???
        case "=" =>
          logger.debug(s"Applying = to $args")
          assert(args.length == 2)
          val ops = args map (arg => qualIdentToPtrExpr(arg, varMap))
          List(Atoms(PtrEq(ops(0), ops(1))))
        case pred if env.preds.contains(pred) =>
          val callArgs = args map (arg => qualIdentToPtrExpr(arg, varMap))
          List(Atoms(PredCall(pred, callArgs)))
        case other =>
          throw new Exception(s"Can't convert $other to symbolic heap")
      }
    case IndexedIdentifier(Symbol("emp"),_) =>
      List(Atoms(Nil, Nil, Nil, Nil))
    case Exists(vars, term) =>
      val qvars: List[String] = vars map (_.asInstanceOf[SortedVar].name.str)
      val qvarMap = qvars.zipWithIndex.map {
        case (str,ix) => (str,Var(-ix-1))
      }
      val extendedMap = varMap ++ qvarMap
      val termAtoms = collectAtoms(term, env, extendedMap)
      assert(termAtoms.length == 1)
      List(termAtoms.head.copy(qvars = qvars))
    case other =>
      throw new Exception(s"Can't convert $other to symbolic heap")
  }

  def constructorToPtrExprs(sid : SidBuilder, env: Env, varMap: Map[String, Var]): Seq[PtrExpr] = sid match {
    case Args((s@Symbol(hd)) :: tl) =>
      if (tl.isEmpty) {
        env.mkPtrTrg(List((qualIdentToPtrExpr(s, varMap), DefaultSel)))
      } else {
        val args = tl map (arg => qualIdentToPtrExpr(arg, varMap))
        val c = env.types.getConstructor(hd)
        val sels = c.sels.map(_.sel.str)
        env.mkPtrTrg(args zip sels)
      }
    case other =>
      throw new Exception(s"Can't convert $other to constructor application")
  }

  def qualIdentToPtrExpr(sid : SidBuilder, varMap: Map[String,Var]): PtrExpr = sid match {
    case Symbol(str) => PtrVar(varMap(str))
    case QualifiedIdentifier(Symbol(str), _) =>
      // We don't care about the type info
      if (str == "nil") NullPtr() else throw new Exception(s"Unexpected qualified identifier $str")
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
          res += DefaultSel
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
