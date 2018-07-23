package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.{NullPtr, PtrExpr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jens on 2/24/17.
  */
case class Model(stack : Map[Var, Loc], heap : Map[Loc,Seq[Loc]]) extends HarrshLogging {

  assert(stack.keySet.isEmpty || Var.minOf(stack.keySet) > Var.nil) // Only non-null free variables are in the stack
  assert(!heap.keySet.contains(Model.NullLoc)) // Null is not allocated
  // The following assertion is violated if there are dangling pointers!
  //assert(stack.values.toSet subsetOf (heap.keySet ++ heap.values.flatten ++ Set(0)))

  override def toString: String = {
    "Stack {\n" + stack.toList.sortWith(_._1 < _._1).map{
      case (v, l) => "  " + v + " -> " + l
    }.mkString("\n") + "\n}\nHeap {\n" + heap.toList.sortBy(_._1).map {
      case (f, t) => "  " + f + " -> " + t.mkString(", ")
    }.mkString("\n") + "\n}"
  }

  def evaluateReducedSymbolicHeap(rsh: SymbolicHeap): Boolean = {
    assert(rsh.isReduced)

    processPointers(rsh.pointers.toSet, stack)match {
      case Some(extendedStack) =>
        logger.debug(s"Extended stack to $extendedStack")
        Model.evalInStack(extendedStack, rsh.pure)
      case None =>
        // The points-to assertions on their own are already inconsistent
        false
    }
  }

  def processPointers(pointersToProcess: Set[PointsTo], extendedStack: Map[Var, Loc]): Option[Map[Var, Loc]] = {
    if (pointersToProcess.isEmpty) {
      Some(extendedStack)
    } else {
      val pto = pointersToProcess.find(pto => extendedStack.contains(pto.from.getVarOrZero)) match {
        case Some(value) => value
        case None =>
          // If there are only pointers with unconstrained left-hand sides, we give up
          // This is fine, because we never run into this case for rooted models / SIDs
          // (Of course the result could still be 'true', so we raise an exception rather than returning 'false'.)
          throw new Exception("Can only evaluate rooted formulas")
      }

      addNamesFromPto(pto, extendedStack).flatMap(processPointers(pointersToProcess - pto, _))
    }
  }

  def addNamesFromPto(pto: PointsTo, extendedStack: Map[Var, Loc]) : Option[Map[Var, Loc]] = {
    for {
      targetLocs <- heap.get(extendedStack(pto.from.getVarOrZero))
      if targetLocs.length == pto.to.length
      varLocPairs = pto.to.map(_.getVarOrZero) zip targetLocs
      extended <- extendStack(extendedStack, varLocPairs)
    } yield extended
  }

  private def extendStack(stack: Map[Var, Loc], targets: Seq[(Var,Loc)]): Option[Map[Var, Loc]] = targets match {
    case (v, l) +: otherTargets =>
      logger.debug(s"Will check whether interpreting $v as $l is consistent with the stack")
      stack.get(v) match {
        case Some(fixedLoc) =>
          if (fixedLoc == l) {
            // Consistent variable identity. Leave stack as is
            extendStack(stack, otherTargets)
          } else {
            // Conflicting locations have to be assigned to variable v => Can't extend stack
            None
          }
        case None =>
          // (Bound) variable not in stack yet => Extend stack...
          val updated = if (v.isNull) {
            // ...unless the variable is null, which we don't add to the stack
            stack
          } else {
            assert(v.isBound)
            stack.updated(v, l)
          }
          extendStack(updated, otherTargets)
      }
    case _ => Some(stack)
  }

}

object Model {
  val empty: Model = Model(Map(), Map())

  val NullLoc : Loc = 0

  def fromTuples(modelDescription: (Loc, Iterable[Loc], Iterable[Var])*): Model = {
    modelDescription.foldLeft(empty)(addTupleToModel)
  }

  private def addTupleToModel(m: Model, t: (Loc, Iterable[Loc], Iterable[Var])): Model = {
    val newHeap = m.heap.updated(t._1, t._2.toSeq)
    val newStack = m.stack ++ t._3.map((_, t._1))
    Model(newStack, newHeap)
  }

  def fromRSH(sh : SymbolicHeap): Option[Model] = {
    if (sh.nonReduced) {
      println("Can't convert non-reduced symbolic heap to model")
      None
    } else {
      val allocAtoms = sh.pointers.map(_.from).map(expr => PtrNEq(expr, NullPtr))
      val diffAtoms = {
        for {
          i <- 0 until sh.pointers.size - 1
          j <- i + 1 until sh.pointers.size
        } yield PtrNEq(sh.pointers(i).from, sh.pointers(j).from)
      }
      val cl: Closure = Closure.ofSetOfAtoms(sh.pure.toSet ++ allocAtoms ++ diffAtoms)

      if (cl.asSetOfAtoms.exists(atom => !atom.isEquality && atom.getVarsWithNull.size == 1)) {
        println("Can't get model for unsatisfiable heap")
        None
      } else {

        val memLayout: Map[Set[Var], Loc] = Map() ++ sh.allVars.groupBy(cl.getEquivalenceClass).keys.zipWithIndex.map {
          case (set, i) => (set, i + 1)
        }

        def varToLoc(v: Var): Loc = memLayout(cl.getEquivalenceClass(v))

        def exprToLoc(v: PtrExpr): Loc = v match {
          case NullPtr => 0
          case PtrVar(id) => varToLoc(id)
        }

        val stack: Map[Var, Loc] = Map() ++ sh.freeVars.map {
          v => (v, varToLoc(v))
        }
        val heap: Map[Loc, Seq[Loc]] = Map() ++ sh.pointers.map {
          ptr => (exprToLoc(ptr.from), ptr.to.map(exprToLoc))
        }

        Some(Model(stack, heap))
      }
    }
  }

  def evalInStack(stack: Map[Var, Loc], pure: PureAtom): Boolean = {
    val leftVar = pure.l.getVarOrZero
    val leftLoc = if (leftVar.isNull) NullLoc else stack(leftVar)
    val rightVar = pure.r.getVarOrZero
    val rightLoc = if (rightVar.isNull) NullLoc else stack(rightVar)
    (leftLoc == rightLoc) == pure.isEquality
  }

  def evalInStack(stack: Map[Var, Loc], pure: Seq[PureAtom]): Boolean = pure.forall(evalInStack(stack, _))
}
