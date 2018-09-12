package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

object SymbolicHeapUtils {

  def splitIntoRootedComponents(sh: SymbolicHeap, sid: SID): List[SymbolicHeap] = {
    assert(sid.isRooted)
    val rootedAtoms = sh.atoms.all map (toRootedAtom(_, sid))
    val reach = transitiveReachability(rootedAtoms, Closure.fromSH(sh))
    val groups = groupByReachability(rootedAtoms, reach)
    assert(groups.flatMap(_.atoms).toSet == sh.atoms.all.toSet)
    groups map (_.toSymbolicHeap)
  }

  private case class RootedAtom(atom: SepLogAtom, root: Var)

  private case class RootedComponent(root: Var, atoms: List[SepLogAtom]) {
    def toSymbolicHeap: SymbolicHeap = {
      // Reverse to get original order of atoms
      SymbolicHeap(atoms.reverse: _*)
    }
  }

  private def toRootedAtom(atom: SepLogAtom, sid: SID): RootedAtom = {
    val root = atom match {
      case PointsTo(from, _) => from
      case PredCall(name, args) => args(sid(name).rootParamIndex.get)
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
