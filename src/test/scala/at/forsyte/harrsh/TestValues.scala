package at.forsyte.harrsh

import at.forsyte.harrsh.TestValues.RichPred
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.seplog.inductive.{PredCall, PureAtom}
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}

import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by jens on 4/7/17.
  */
trait TestValues {

  def P(s: String): RichPred = new RichPred(s)

  val (x1,x2,x3,x4,x5,x6) = (Var.defaultFV(1), Var.defaultFV(2), Var.defaultFV(3), Var.defaultFV(4), Var.defaultFV(5), Var.defaultFV(6))
  val (y1,y2,y3,y4,y5,y6,y7,y8,y9) = (BoundVar(1),BoundVar(2),BoundVar(3),BoundVar(4),BoundVar(5),BoundVar(6),BoundVar(7),BoundVar(8),BoundVar(9))
  val nil = NullConst

  case class Fvs(xs: FreeVar*){
    def toSeq: Seq[FreeVar] = xs
  }
  case class Alloc(xs: Var*) {
    def toSet: Set[Var] = xs.toSet
  }
  case class Eqs(eqs: PureAtom*) {
    def toSet: Set[PureAtom] = eqs.toSet
  }
  case class Reach(pairs: (Var,Var)*) {
    def toRM: ReachabilityMatrix = ReachabilityMatrix.fromPairs(pairs)
  }

  def TI(fvs: Fvs, alloc: Alloc) = TrackingInfo(alloc.toSet, Set(), fvs.toSeq)
  def TI(fvs: Fvs, alloc: Alloc, pure: Eqs) = TrackingInfo(alloc.toSet, pure.toSet, fvs.toSeq)
  def TI(fvs: Fvs, pure: Eqs) = TrackingInfo(Set(), pure.toSet, fvs.toSeq)
  def RI(fvs: Fvs, alloc: Alloc, mx: Reach) = ReachabilityInfo(TrackingInfo(alloc.toSet, Set(), fvs.toSeq), mx.toRM)
  def RI(fvs: Fvs, alloc: Alloc, pure: Eqs, mx: Reach) = ReachabilityInfo(TrackingInfo(alloc.toSet, pure.toSet, fvs.toSeq), mx.toRM)
  def RI(fvs: Fvs, pure: Eqs, mx: Reach) = ReachabilityInfo(TrackingInfo(Set(), pure.toSet, fvs.toSeq), mx.toRM)
  def TI(fvs: Fvs, alloc: Alloc, flag: Boolean) = (TrackingInfo(alloc.toSet, Set(), fvs.toSeq), flag)
  def TI(fvs: Fvs, alloc: Alloc, pure: Eqs, flag: Boolean) = (TrackingInfo(alloc.toSet, pure.toSet, fvs.toSeq), flag)
  def TI(fvs: Fvs, pure: Eqs, flag: Boolean) = (TrackingInfo(Set(), pure.toSet, fvs.toSeq), flag)
  def RI(fvs: Fvs, alloc: Alloc, mx: Reach, flag: Boolean) = (ReachabilityInfo(TrackingInfo(alloc.toSet, Set(), fvs.toSeq), mx.toRM), flag)
  def RI(fvs: Fvs, alloc: Alloc, pure: Eqs, mx: Reach, flag: Boolean) = (ReachabilityInfo(TrackingInfo(alloc.toSet, pure.toSet, fvs.toSeq), mx.toRM), flag)
  def RI(fvs: Fvs, pure: Eqs, mx: Reach, flag: Boolean) = (ReachabilityInfo(TrackingInfo(Set(), pure.toSet, fvs.toSeq), mx.toRM), flag)

}

object TestValues {

  val DefaultTestTimeout = Duration(60, SECONDS)

  class RichPred(val name: String) extends AnyVal {
    def apply(args: Var*): PredCall = PredCall(name, args)
  }

}