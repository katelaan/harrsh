package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.seplog.{BoundVar, Var}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

object SplitIntoRootedComponents {

  def apply(sh: SymbolicHeap, sid: RichSid): List[SymbolicHeap] = {
    assert(sid.isRooted)
    if (sh.predCalls.isEmpty && !sh.hasPointer) {
      // Only perform reachability analysis if there is anything to analyze
      List(sh)
    } else {
      val rootedAtoms = sh.atoms.all map (toRootedAtom(_, sid))
      val reach = transitiveReachability(rootedAtoms, Closure.fromSH(sh))
      val groups = groupByReachability(rootedAtoms, reach)
      assert(groups.flatMap(_.atoms).toSet == sh.atoms.all.toSet)
      if (sharedBoundVars(groups)) {
        throw new IllegalArgumentException(s"Currently no support for queries in which bound variables are shared between connected components, but SCCs are ${groups.mkString(", ")}")
      }
      val rootedSHs = groups map (_.toSymbolicHeap)
      integrateEmptyComponents(rootedSHs)
    }
  }

  private def integrateEmptyComponents(rootedSHs: List[SymbolicHeap]): List[SymbolicHeap] = {
    if (rootedSHs.size == 1) rootedSHs
    else {
      val (empty,nonempty) = rootedSHs.partition(sh => sh.predCalls.isEmpty && !sh.hasPointer)
      // FIXME: Is it really a good idea to add the atoms to an arbitrary component? Shouldn't we add it to one which uses  at least one of the involved vars if possible?
      if (empty.isEmpty) nonempty else addPureConstraints(nonempty.head, empty) :: nonempty.tail
    }
  }

  private def addPureConstraints(nonemptySh: SymbolicHeap, emptyShs: Seq[SymbolicHeap]): SymbolicHeap = {
    val allPureConstraints = nonemptySh.pure ++ emptyShs.flatMap(_.pure)
    val fvs = (nonemptySh.freeVars ++ emptyShs.flatMap(_.freeVars)).distinct
    nonemptySh.copy(pure = allPureConstraints, freeVars = fvs)
  }

  private def sharedBoundVars(groups: List[RootedComponent]): Boolean = {
    if (groups.size == 1) {
      false
    }
    else {
      val boundVars = groups map (_.boundVars)
      boundVars.tail.fold(boundVars.head)(_ intersect _).nonEmpty
    }
  }

  private case class RootedAtom(atom: SepLogAtom, root: Var)

  private case class RootedComponent(root: Var, atoms: List[SepLogAtom]) {
    def toSymbolicHeap: SymbolicHeap = {
      // Reverse to get original order of atoms
      SymbolicHeap(atoms.reverse: _*)
    }

    def boundVars: Set[BoundVar] = Var.boundVars(atoms.flatMap(_.getVars).toSet ++ Set(root))

    override def toString: String = s"[$root --> ${atoms.mkString(" * ")}]"
  }

  private def toRootedAtom(atom: SepLogAtom, sid: RichSid): RootedAtom = {
    val root = atom match {
      case PointsTo(from, _) => from
      case PredCall(name, args) => args(sid.rootParamIndex(name))
      case PureAtom(l, r, _) =>
        // For pure atoms, we arbitrarily choose a non-null root;
        // in case the pointers/calls of the compared vars end up in different components, the disequality will
        // become part of the first component
        if (l.isNull) r else l
    }
    RootedAtom(atom, root)
  }

  private def groupByReachability(atoms: Seq[RootedAtom], reach: Map[Var,Set[Var]]): List[RootedComponent] = {
    groupByReachability(atoms, reach, Set.empty).toList
  }

  @tailrec private def groupByReachability(atoms: Seq[RootedAtom], reach: Map[Var,Set[Var]], groups: Set[RootedComponent]): Set[RootedComponent] = {
    atoms match {
      case hd +: tl =>
        val extendedGroups = addToGroup(hd, groups, reach)
        groupByReachability(tl, reach, extendedGroups)
      case _ => groups
    }
  }

  private def addToGroup(atom: RootedAtom, groups: Set[RootedComponent], reach: Map[Var, Set[Var]]): Set[RootedComponent] = {
    groups.find(groupContainsAtom(_, atom, reach)) match {
      case Some(groupWithoutNewAtom) =>
        val groupWithNewAtom = groupWithoutNewAtom.copy(atoms = atom.atom +: groupWithoutNewAtom.atoms)
        groups - groupWithoutNewAtom + groupWithNewAtom
      case None =>
        groups.find(groupReachableFromAtom(_, atom, reach)) match {
          case Some(reachableGroup) =>
            val groupWithNewAtom = RootedComponent(atom.root, atom.atom +: reachableGroup.atoms)
            groups - reachableGroup + groupWithNewAtom
          case None =>
            groups + RootedComponent(atom.root, List(atom.atom))
        }
    }
  }

  private def groupContainsAtom(component: RootedComponent, atom: RootedAtom, reach: Map[Var, Set[Var]]): Boolean = {
    reach.getOrElse(component.root, Set.empty).contains(atom.root)
  }

  private def groupReachableFromAtom(component: RootedComponent, atom: RootedAtom, reach: Map[Var, Set[Var]]): Boolean = {
    reach.getOrElse(atom.root, Set.empty).contains(component.root)
  }

  private def transitiveReachability(atoms: Seq[RootedAtom], closure: Closure): Map[Var,Set[Var]] = {
    val oneStepReach = rootedAtomsToReachabilityPairs(atoms, closure)
    val groupedBySource = oneStepReach.groupBy(_._1).mapValues(_.map(_._2))
    Combinators.fixedPointOfMap(groupedBySource)
  }

  private def rootedAtomsToReachabilityPairs(atoms: Seq[RootedAtom], closure: Closure): Set[(Var,Var)] = {
    atoms.toSet[RootedAtom] flatMap (rootedAtomToReachabilityPairs(_, closure))
  }

  private def rootedAtomToReachabilityPairs(atom: RootedAtom, closure: Closure): Set[(Var,Var)] = {
    atom match {
      case RootedAtom(PureAtom(_, _, false), _) =>
        // Disequality => Says nothing about reachability
        Set.empty
      case _ =>
        for {
          src <- closure.getEquivalenceClass(atom.root)
          arg <- atom.atom.getVars
          trg <- closure.getEquivalenceClass(arg)
        } yield (src, trg)
    }
  }

}
