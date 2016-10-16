package slex.heapautomata

import slex.Combinators
import slex.main._
import slex.seplog.{NullPtr, PointsTo, PtrEq, PtrExpr, PtrNEq, SpatialAtom, SymbolicHeap}

import scala.annotation.tailrec

/**
  * Created by jens on 10/16/16.
  */
object TrackingAutomata {

  type FV = Int

  def fv(ix : Int) = ExampleSIDs.fv(ix)

  def unFV(x : String) : Int = if (x == NullPtr().toString) 0 else Integer.valueOf(x.tail)

  case class FVEquality(left : FV, right : FV, isEqual : Boolean) {

    if (left >= right) throw new IllegalStateException("FVEquality invariant " + left + " < " + right + "violated")

    override def toString = left + (if (isEqual) " \u2248 " else " \u2249 ") + right

  }

  def makeFVEquality(left : PtrExpr, right : PtrExpr, isEqual : Boolean): FVEquality = makeFVEquality(unFV(left.toString), unFV(right.toString), isEqual)

  def makeFVEquality(left : FV, right : FV, isEqual : Boolean): FVEquality = {
    if (left < right) new FVEquality(left, right, isEqual) else new FVEquality(right, left, isEqual)
  }

  // TODO This method is ridiculously inefficient, because we compute one copy of each eq class for each member of the class
  def computeRepresentationsInClosure(pure : Set[FVEquality]) : FV => Boolean = i => {

    var mapToClasses : Map[FV,Set[FV]] = Map()

    def extendEntry(key : FV, newVal : FV) = {
      val eqClass = if (mapToClasses.isDefinedAt(key)) {
        // Class is already defined, just add the new value
        mapToClasses(key) + newVal
      } else {
        // Key not in any known eq class yet
        // Either have to extend class for val, if known already, or create new class
        if (mapToClasses.isDefinedAt(newVal)) mapToClasses(newVal) + key else Set(key, newVal)
      }

      // Extend entry for all members of the eq class
      for {
        classMember <- eqClass
      } {
        mapToClasses = mapToClasses + (classMember -> eqClass)
      }
    }

    for {
      FVEquality(left, right, isEqual) <- pure
      if isEqual
    } {
      extendEntry(left, right)

      //      println("Inserting " + left + " -> " + right)
      //      for {
      //        (key, vals) <- mapToClasses
      //      } println(key + " --> " + vals)
    }

    // If the EQ class is defined, check if i is the representation = the minimum of that class
    // Otherwise, no equality for i has been set, so i is the unique and hence minimal element, so it is the representation
    if (mapToClasses.isDefinedAt(i)) mapToClasses(i).min == i else true
  }

  def allFVEqualities(numFV : Int) : Set[FVEquality] = {
    for {
      i <- Set() ++ (1 to numFV)
      j <- Set() ++ (0 to numFV)
      if i > j
      eq <- Set(true, false)
    } yield FVEquality(i, j, eq)
  }

  def powerSet[A](set : Set[A]) : Set[Set[A]] = {
    val seq = set.toSeq

    // TODO: Rewrite to tailrec
    def powerSet(elems : Seq[A]) : Set[Set[A]] = {
      if (elems.isEmpty)
        Set(Set())
      else {
        val newelem = elems.head
        val smaller = powerSet(elems.tail)
        smaller flatMap (set => Set(set, set + newelem))
      }
    }

    powerSet(seq)
  }

  def trackingAutomaton(numFV : Int, alloc : Set[FV], pure : Set[FVEquality]) = new HeapAutomaton with SlexLogging {

    override val description: String = "TRACK(" + numFV + ")"

    private lazy val allFVs = 0 to numFV
    private lazy val allEQs = allFVEqualities(numFV)

    override type State = (Set[FV], Set[FVEquality])

    override lazy val states: Set[State] = {
      for {
        alloc <- powerSet(Set() ++ (1 to numFV))
        pure <- powerSet(allEQs)
      } yield (alloc, pure)
    }

    override def isFinal(s: State): Boolean = s._1 == alloc && s._2 == pure

    // TODO Restriction regarding number of FVs
    override def isDefinedOn(lab: SymbolicHeap): Boolean = true

    private def representationSH(s : State) : SymbolicHeap = {

      val pure = s._2 map {
        case FVEquality(left, right, isEqual) =>
          if (isEqual) PtrEq(fv(left), fv(right)) else PtrNEq(fv(left), fv(right))
      }

      val isRepresentant = computeRepresentationsInClosure(s._2)

      val nonredundantAlloc = s._1 filter (isRepresentant(_))

      val alloc : Set[SpatialAtom] = nonredundantAlloc map (i => PointsTo(fv(i), NullPtr()))

      val res = SymbolicHeap(pure.toSeq, alloc.toSeq)

      logger.debug("Converting " + s + " to " + res)

      res
    }

    private def shrink(sh : SymbolicHeap, qs : Seq[State]) : SymbolicHeap = {
      val shFiltered = sh.removeCalls
      val newHeaps = qs map representationSH
      val combined = SymbolicHeap.combineAllHeaps(shFiltered +: newHeaps)
      combined
    }

    override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
      val shrunk = shrink(lab, src)
      logger.debug("Shrunk " + lab + " into " + shrunk)

      // Compute allocation set and equalities for shrunk and compare to target
      val allocExplicit: Seq[FV] = shrunk.pointers map (p => unFV(p.from.toString))
      val pureExplicit : Set[FVEquality] =  Set() ++ shrunk.ptrEqs map {
        case PtrEq(l, r) => makeFVEquality(l, r, true)
        case PtrNEq(l, r) => makeFVEquality(l, r, false)
        case _ => throw new IllegalStateException("Filtering on pointers failed")
      }

      // Add inequalities for allocated variables
      val inequalitiesFromAlloc : Seq[FVEquality] = Combinators.square(allocExplicit) map {
        case (l,r) => makeFVEquality(l, r, false)
      }
      val pureWithAlloc : Set[FVEquality] = pureExplicit ++ inequalitiesFromAlloc

      // Compute fixed point of inequalities
      val allPure = propagateConstraints(pureWithAlloc)

      // Propagate equalities to allocation info
      val allocFromEqualities : Set[FV] = propagateEqualitiesToAlloc(allocExplicit.toSet, allPure)

      val shrunkState : State = (allocFromEqualities, allPure)
      logger.debug("State for shrunk SH: " + shrunkState)
      val res = shrunkState == trg

      logger.debug("Transition " + src.mkString(", ") + "--[" + lab + "]-->" + trg + " : " + res)
      res
    }

  }

  @tailrec
  private def propagateConstraints(from : Set[FVEquality]): Set[FVEquality] = {
    // TODO This is inefficient as well

    val newEqs : Seq[FVEquality] = (Combinators.square(from.toIndexedSeq) map {
      case (l,r) => transitiveConstraint(l ,r)
    } filter(_.isDefined) map(_.get))

    val combined = from ++ newEqs
    if (combined == from) from else propagateConstraints(combined)

  }

  private def transitiveConstraint(fv1 : FVEquality, fv2 : FVEquality) : Option[FVEquality] = {

    if (fv1.isEqual || fv2.isEqual) {
      // If at least one is an equality, and one end of the eqs coincides, we can propagate

      val newPair: Option[(FV, FV)] =
        if (fv1.left == fv2.left) Some((fv1.right, fv2.right))
        else if (fv1.left == fv2.right) Some((fv1.right, fv2.left))
        else if (fv1.right == fv2.left) Some((fv1.left, fv2.right))
        else if (fv1.right == fv2.right) Some((fv1.left, fv2.left))
        else None

      newPair map (p => makeFVEquality(p._1, p._2, fv1.isEqual && fv2.isEqual))
    }
    else {
      // Can't infer anything if both are inequalities
      None
    }

  }

  private def propagateEqualitiesToAlloc(explicit: Set[FV], allPure: Set[FVEquality]): Set[FV] = {
    // TODO This is inefficient as well
    val propagated : Set[FV] = (for {
      FVEquality(l, r, isEq) <- allPure
      if isEq
      if explicit.contains(l) || explicit.contains(r)
    } yield Set(l,r)).flatten

    explicit union propagated
  }

}
