package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{NullPtr, PtrExpr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive._

import scala.annotation.tailrec

/**
  * Created by jens on 2/24/17.
  */
object ModelToFormula {

  def apply(model : Model) : SymbolicHeap = {

    // Get a map from locations to vars
    // Note that if multiple vars point to the same loc, we will lose this info here
    val locToVar : Map[Loc,Var] = Map() ++ model.stack.toSeq.map(p => (p._2,p._1)) ++ Map(0 -> Var(0))

    val (pointers,boundVars) : (Seq[PointsTo],Set[Var]) = generatePointers(locToVar, model.heap.toList)

    // Add equalities for variables that map to the same location
    // Recall that we previously lost that info when building the locToVar map
    val groupByLocs = model.stack.toList.groupBy(_._2)
    val varGroups : Iterable[List[Var]] = groupByLocs.values.map(ls => ls.map(_._1))
    val varEqualities : Seq[PureAtom] = (varGroups flatMap equalities).toSeq

    // Add equalities to null
    val nullVars = model.stack.filter(pair => pair._2 == 0).keySet.toSeq
    val nullEqualities : Seq[PureAtom] = nullVars map (v => PtrEq(PtrVar(v), NullPtr()).asInstanceOf[PureAtom])

    SymbolicHeap.fromFullDescription(varEqualities ++ nullEqualities, pointers, List.empty, model.stack.size, boundVars)
  }

  private def equalities(vs : List[Var]) : Seq[PureAtom] = vs match {
    case fst :: snd :: tl => PtrEq(PtrVar(fst), PtrVar(snd)) +: equalities(snd :: tl)
    case _ => Seq.empty
  }

  private def generatePointers(locToVar : Map[Loc,Var], heap : List[(Loc,Seq[Loc])]) : (Seq[PointsTo], Set[Var]) = {
    val (reversedPointers, updatedMap) = generatePointers(locToVar, heap, Nil)
    //println(reversedPointers.mkString(","))
    //println(updatedMap.mkString(","))
    (reversedPointers.reverse, updatedMap.values.filter(_.isBound).toSet)
  }

  @tailrec private def generatePointers(locToVar : Map[Loc,Var], heap : List[(Loc,Seq[Loc])], acc : List[PointsTo]) : (Seq[PointsTo], Map[Loc,Var]) = heap match {
    case Nil => (acc, locToVar)
    case (src,trg) :: tail =>
      val updatedVars = introduceVars(locToVar, src +: trg)
      val mkPtr = locToPtrExpr(updatedVars)_
      val ptr = PointsTo(mkPtr(src), trg map mkPtr)
      generatePointers(updatedVars, tail, ptr :: acc)
  }

  private def locToPtrExpr(locToVar : Map[Loc,Var])(loc : Loc) : PtrExpr = {
    if (loc == 0) NullPtr() else PtrVar(locToVar(loc))
  }

  /**
    * Makes sure there is a variable for each of the given locations, adding new variables if necessary
    * @param locToVar Input map of locations to vars
    * @param locs Locations to be identified by vars
    * @return Updated map
    */
  @tailrec private def introduceVars(locToVar : Map[Loc,Var], locs : Seq[Loc]) : Map[Loc,Var] = {
    if (locs.isEmpty) {
      locToVar
    } else {
      val loc = locs.head
      val updated = if (locToVar.isDefinedAt(loc)) locToVar else addVarForLoc(locToVar, loc)
      introduceVars(updated, locs.tail)
    }
  }

  private def addVarForLoc(locToVar : Map[Loc,Var], loc : Loc) : Map[Loc,Var] = {
    val min = Var.minOf(locToVar.values.toSeq :+ Var.nil)
    val newVar = min - 1
    locToVar + (loc -> newVar)
  }

}
