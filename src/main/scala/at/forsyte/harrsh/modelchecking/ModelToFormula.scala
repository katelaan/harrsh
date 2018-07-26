package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}
import at.forsyte.harrsh.seplog.inductive._

import scala.annotation.tailrec

/**
  * Created by jens on 2/24/17.
  */
object ModelToFormula extends HarrshLogging {

  def apply(model : Model) : SymbolicHeap = {

    // Get a map from locations to vars
    // Note that if multiple vars point to the same loc, we will lose this info here
    val locToVar : Map[Loc,Var] = Map() ++ model.stack.toSeq.sortBy(_._1).reverse.map(p => (p._2,p._1)) ++ Map(0 -> NullConst)

    val (pointers,boundVars) : (Seq[PointsTo],Set[BoundVar]) = generatePointers(locToVar, model.heap.toList)

    // Add equalities for variables that map to the same location
    // Recall that we previously lost that info when building the locToVar map
    val groupByLocs = model.stack.toList.groupBy(_._2)
    val varGroups : Iterable[List[Var]] = groupByLocs.values.map(ls => ls.map(_._1))
    logger.warn(s"After grouping: $varGroups")
    val varEqualities : Seq[PureAtom] = (varGroups flatMap equalities).toSeq

    // Add equalities to null
    val nullVars = model.stack.filter(pair => pair._2 == 0).keySet.toSeq
    val nullEqualities : Seq[PureAtom] = nullVars map (v => PtrEq(v, NullConst))

    val freeVars = model.stack.keys.map(_.asInstanceOf[FreeVar]).toSeq
    SymbolicHeap(varEqualities ++ nullEqualities, pointers, Seq.empty, freeVars)
  }

  private def equalities(vs : List[Var]) : Seq[PureAtom] = vs match {
    case fst :: snd :: tl => PtrEq(fst, snd) +: equalities(snd :: tl)
    case _ => Seq.empty
  }

  private def generatePointers(locToVar : Map[Loc,Var], heap : List[(Loc,Seq[Loc])]) : (Seq[PointsTo], Set[BoundVar]) = {
    val (reversedPointers, updatedMap) = generatePointers(locToVar, heap, Nil)
    //println(reversedPointers.mkString(","))
    //println(updatedMap.mkString(","))
    (reversedPointers.reverse, updatedMap.values.filter(_.isBound).map(_.asInstanceOf[BoundVar]).toSet)
  }

  @tailrec private def generatePointers(locToVar : Map[Loc,Var], heap : List[(Loc,Seq[Loc])], acc : List[PointsTo]) : (Seq[PointsTo], Map[Loc,Var]) = heap match {
    case Nil => (acc, locToVar)
    case (src,trg) :: tail =>
      val updatedVars = introduceVars(locToVar, src +: trg)
      val mkPtr = locToFreeVarOrNull(updatedVars)_
      val ptr = PointsTo(mkPtr(src), trg map mkPtr)
      generatePointers(updatedVars, tail, ptr :: acc)
  }

  private def locToFreeVarOrNull(locToVar : Map[Loc,Var])(loc : Loc) : Var = {
    if (loc == 0) NullConst else locToVar(loc)
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
    val newVar = Var.getNextUnusedBoundVar(locToVar.values)
    locToVar + (loc -> newVar)
  }

}
