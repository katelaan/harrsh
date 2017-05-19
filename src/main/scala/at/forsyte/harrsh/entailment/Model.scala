package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.pure.{Closure}
import at.forsyte.harrsh.seplog.{NullPtr, PtrExpr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PtrNEq, SymbolicHeap}

/**
  * Created by jens on 2/24/17.
  */
case class Model(stack : Map[Var, Loc], heap : Map[Loc,Seq[Loc]]) {

  assert(stack.keySet.isEmpty || stack.keySet.min > 0) // Only non-null free variables are in the stack
  assert(!heap.keySet.contains(0)) // Null is not allocated
  // The following assertion is violated if there are dangling pointers!
  //assert(stack.values.toSet subsetOf (heap.keySet ++ heap.values.flatten ++ Set(0)))

  override def toString: String = {
    "Stack {\n" + stack.toList.sortBy(_._1).map{
      case (v, l) => "  " + Var.toDefaultString(v) + " -> " + l
    }.mkString("\n") + "\n}\nHeap {\n" + heap.toList.sortBy(_._1).map {
      case (f, t) => "  " + f + " -> " + t.mkString(", ")
    }.mkString("\n") + "\n}"
  }

}

object Model {
  val empty: Model = Model(Map(), Map())

  def fromRSH(sh : SymbolicHeap): Option[Model] = {
    if (sh.nonReduced) {
      println("Can't convert non-reduced symbolic heap to model")
      None
    } else {
      val allocAtoms = sh.pointers.map(_.from).map(expr => PtrNEq(expr, NullPtr()))
      val diffAtoms = {
        for {
          i <- 0 until sh.pointers.size - 1
          j <- i + 1 until sh.pointers.size
        } yield PtrNEq(sh.pointers(i).from, sh.pointers(j).from)
      }
      val cl: Closure = Closure.ofSetOfAtoms(sh.pure.toSet ++ allocAtoms ++ diffAtoms)

      if (cl.asSetOfAtoms.exists(atom => atom.isInstanceOf[PtrNEq] && atom.getVarsWithNull.size == 1)) {
        println("Can't get model for unsatisfiable heap")
        None
      } else {

        val memLayout: Map[Set[Var], Loc] = Map() ++ sh.allVars.groupBy(cl.getEquivalenceClass).keys.zipWithIndex.map {
          case (set, i) => (set, i + 1)
        }

        def varToLoc(v: Var): Loc = memLayout(cl.getEquivalenceClass(v))

        def exprToLoc(v: PtrExpr): Loc = v match {
          case NullPtr() => 0
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
}
