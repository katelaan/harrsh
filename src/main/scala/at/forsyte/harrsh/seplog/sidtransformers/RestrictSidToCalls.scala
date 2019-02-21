package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.seplog.inductive._

object RestrictSidToCalls {

  def apply(sid: SID, calls: Set[PredCall]): SID = {
    val restrictor = (a: SID, preds: Set[String]) => {
        a.copy(startPred = "UNDEFINED",
          preds = sid.preds.filter(pred => preds.contains(pred.head)))
    }
    restrictToCalls(sid, calls, restrictor)
  }

  def apply(sid: RichSid, calls: Set[PredCall]): RichSid = {
    val restrictor = (a: RichSid, preds: Set[String]) => {
      a.copy(startPred = "UNDEFINED",
        preds = sid.preds.filter(pred => preds.contains(pred.head)),
        roots = sid.roots.filterKeys(pred => preds.contains(pred)))
    }
    restrictToCalls(sid, calls, restrictor)
  }

  private def restrictToCalls[A <: SidLike](sid: A, calls: Set[PredCall], restrictor: (A, Set[String]) => A): A = {
    val predsByCall: Set[(String, Set[String])] = calls.map(_.name).map(p => (p, getReachablePreds(sid, p)))
    predsByCall find (_._2.isEmpty) match {
      case Some(value) =>
        throw new IllegalArgumentException(s"Illegal specification: The SID doesn't contain any rules for ${value._1}")
      case None =>
        // There are rules for all predicates => Filter SID accordingly & return
        val uniquePreds = predsByCall.flatMap(_._2)
        restrictor(sid, uniquePreds)
    }
  }

  private def getReachablePreds(sid: SidLike, curr: String, visited: Set[String] = Set.empty): Set[String] = {
    if (visited.contains(curr)) visited
    else {
      val withCurr = visited + curr
      val occurringPreds = sid(curr).rules.toSet[RuleBody].flatMap(_.body.predCalls).map(_.name)
      val reachableFromOccurring = occurringPreds.flatMap(getReachablePreds(sid, _, withCurr))
      // Note: Need to explicitly include withCurr in result because reachableFromOccurring may be empty!
      withCurr ++ reachableFromOccurring
    }
  }

  }
