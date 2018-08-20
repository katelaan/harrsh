package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.Var

package object entailment {

  type NodeId = Int

  object NodeId {

    def freshIds(usedIds: Set[NodeId], numIds: Int): Seq[NodeId] = {
      val maxUsed = if (usedIds.nonEmpty) usedIds.max else -1
      maxUsed + 1 to maxUsed + numIds
    }

    def zero: NodeId = 0

  }

  type Unification = Seq[Set[Var]]

  type SubstitutionUpdate = Var => Set[Var]

  object SubstitutionUpdate {

    def fromPairs(pairs: Seq[(Var,Var)]) : SubstitutionUpdate = {
      val map = pairs.toMap
      v => Set(map.getOrElse(v, v))
    }

  }

  type VarUsage = Seq[Boolean]

}
