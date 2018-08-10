package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.FreeVar

package object entailment {

  type NodeId = Int

  object NodeId {

    def freshIds(usedIds: Set[NodeId], numIds: Int): Seq[NodeId] = {
      val maxUsed = usedIds.max
      maxUsed + 1 to maxUsed + numIds
    }

    def zero: NodeId = 0

  }

  type Unification = Seq[Set[FreeVar]]

}
