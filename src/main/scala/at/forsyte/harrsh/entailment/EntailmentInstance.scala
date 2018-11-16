package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive._

case class EntailmentInstance(lhsSid: SID, lhsCall: PredCall, rhsSid: SID, rhsCall: PredCall, entailmentHolds: Option[Boolean])
