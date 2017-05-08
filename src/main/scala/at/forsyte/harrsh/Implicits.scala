package at.forsyte.harrsh

import at.forsyte.harrsh.entailment.{GreedyUnfoldingModelChecker, Model}
import at.forsyte.harrsh.main.MainIO
import at.forsyte.harrsh.parsers.SIDParsers
import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.refinement.DecisionProcedures.AnalysisResult
import at.forsyte.harrsh.refinement.{AutomatonTask, DecisionProcedures, RefinementAlgorithms, RunSat}
import at.forsyte.harrsh.seplog.inductive.{Rule, SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.util.IOUtils

import scala.concurrent.duration
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

/**
  * Created by jens on 4/7/17.
  */
object Implicits {

  private val InteractiveTimeout = Duration(30, duration.SECONDS)

  class ParsableString(val s : String) {

    def load() : SID = {
      IOUtils.findFileIn(s, Defaults.PathsToExamples) match {
        case Some(file) => MainIO.getSidFromFile(file)
        case None =>
          IOUtils.printWarningToConsole("Could not find file '" + s + "' in current path " + Defaults.PathsToExamples.mkString(":"))
          SID.empty("fail")
      }
    }

    def parseModel() : Model =  {
      IOUtils.findFileIn(s, Defaults.PathsToExamples) match {
        case Some(file) => MainIO.getModelFromFile(file)
        case None =>
          IOUtils.printWarningToConsole("Could not find file '" + s + "' in current path " + Defaults.PathsToExamples.mkString(":"))
          Model.empty
      }
    }

    def parse : SymbolicHeap = {
      SIDParsers.CombinedSIDParser.runOnSymbolicHeap(s) match {
        case Some(sh) => sh
        case None =>
          IOUtils.printWarningToConsole("Could not parse '" + s + "' as symbolic heap")
          SymbolicHeap.empty
      }
    }

    def parseSID : SID = {
      SIDParsers.CombinedSIDParser.runOnSID(s) match {
        case Some(sh) => sh
        case None =>
          IOUtils.printWarningToConsole("Could not parse '" + s + "' as SID")
          SID.empty("DUMMY")
      }
    }
  }

  class RichSID(val sid : SID) {

    def refined(task : AutomatonTask) : SID = refineAndCheckEmptiness(task)._1

    def refineAndCheckEmptiness(task : AutomatonTask) : (SID,Boolean) = {
      RefinementAlgorithms.refineSID(sid, task.getAutomaton(sid.numFV), InteractiveTimeout, reportProgress = Defaults.reportProgress) match {
        case Some(refinedSID) =>
          refinedSID
        case None =>
          IOUtils.printWarningToConsole("Refinement failed")
          (SID.empty(sid.startPred),true)
      }
    }

    def hasEmptyIntersectionWithLanguageOf(task : AutomatonTask) : Boolean = {
      val AnalysisResult(isEmpty, analysisTime, timedOut) = DecisionProcedures.decideInstance(sid, task.getAutomaton(sid.numFV), InteractiveTimeout, verbose = Defaults.reportProgress, reportProgress = Defaults.reportProgress)
//      if (timedOut) {
//        println("Reached timeout of " + InteractiveTimeout)
//      } else {
//        println("Finished analysis in " + analysisTime + "ms")
//      }
      isEmpty
    }

    def forall(task : AutomatonTask) : Boolean = hasEmptyIntersectionWithLanguageOf(task.complement)

    def exists(task : AutomatonTask) : Boolean = !hasEmptyIntersectionWithLanguageOf(task)

    def witness : Option[SymbolicHeap] = {
      if (sid.rulesAsHeadToBodyMap.keySet.contains(sid.startPred)) {
        // There is a rule for the start predicate. Assume this means non-emptiness (also this is not necessarily true)
        // TODO: Perform an actual emptiness test here?
        Some(SIDUnfolding.firstReducedUnfolding(sid))
      } else None

    }

    def witness(task : AutomatonTask) : Option[SymbolicHeap] = {
      val (refined, isEmpty) = refineAndCheckEmptiness(task)
        if (isEmpty) None else refined.witness
    }

    def unfoldings(depth : Int) : Iterable[SymbolicHeap] = sid.callToStartPred.unfoldings(sid, depth)
    def reducedUnfoldings(depth : Int) : Iterable[SymbolicHeap] = sid.callToStartPred.reducedUnfoldings(sid, depth)
    def getSomeReducedUnfolding(depth : Int) : SymbolicHeap = sid.callToStartPred.reducedUnfoldings(sid, depth).last

    def getModel: Option[Model] = witness flatMap (_.getModel)
    def getModelAtDepth(depth : Int): Option[Model] = getSomeReducedUnfolding(depth).getModel(sid)

    def baseRule : Rule = {
      val base = sid.rules.filter(!_.body.hasPredCalls)
      if (base.size > 1) {
        IOUtils.printWarningToConsole("Warning: More than one base rule. Will pick arbitrary one")
      }
      base.head
    }

    def recursiveRule : Rule = {
      val rec = sid.rules.filter(_.body.hasPredCalls)
      if (rec.size > 1) {
        IOUtils.printWarningToConsole("Warning: More than one recursive rule. Will pick arbitrary one")
      }
      rec.head
    }
  }

  class RichSymbolicHeap(val sh : SymbolicHeap) {

    def unfoldFirstCall(by : SymbolicHeap) : SymbolicHeap = sh.replaceCall(sh.predCalls.head, by)
    def unfoldSecondCall(by : SymbolicHeap) : SymbolicHeap = sh.replaceCall(sh.predCalls(1), by)
    def unfoldIthCall(i : Int, by : SymbolicHeap) : SymbolicHeap = sh.replaceCall(sh.predCalls(i-1), by)
    def unfoldCalls(by : SymbolicHeap*) : SymbolicHeap = sh.replaceCalls(by)
    def unfoldAllCallsBy(by : SymbolicHeap) : SymbolicHeap = sh.replaceCalls(Seq.fill(sh.predCalls.size)(by))

    def unfoldOnce(sid : SID) : Iterable[SymbolicHeap] = SIDUnfolding.unfoldOnce(sid, Seq(sh))
    def unfoldings(sid : SID, depth : Int) : Iterable[SymbolicHeap] = SIDUnfolding.unfold(sid, depth)
    def reducedUnfoldings(sid : SID, depth : Int) : Iterable[SymbolicHeap] = SIDUnfolding.unfold(sid, depth, reducedOnly = true)

    def simplify : SymbolicHeap = EqualityBasedSimplifications.fullEqualitySimplification(sh)

    def isA(sid : SID) : Boolean = {
      println("Checking " + sh + " |= " + sid.callToStartPred)
      GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(sh, sid.callToStartPred, sid, Defaults.reportProgress)
    }

    def refineBy(sid: SID, task : AutomatonTask) : (SID,Boolean) = {
      SID.fromTopLevelSH(sh, sid).refineAndCheckEmptiness(task)
    }

    def exists(sid: SID, task : AutomatonTask) : Boolean = {
      SID.fromTopLevelSH(sh, sid).exists(task)
    }

    def exists(task : AutomatonTask) : Boolean = {
      if (sh.predCalls.nonEmpty) throw new Throwable("Can't decide properties of non-reduced heaps without reference to an SID")
      SID.fromTopLevelSH(sh, SID.empty("X")).exists(task)
    }

    def forall(sid: SID, task : AutomatonTask) : Boolean = {
      SID.fromTopLevelSH(sh, sid).forall(task)
    }

    def forall(task : AutomatonTask) : Boolean = {
      if (sh.predCalls.nonEmpty) throw new Throwable("Can't decide properties of non-reduced heaps without reference to an SID")
      SID.fromTopLevelSH(sh, SID.empty("X")).forall(task)
    }

    def isSat(sid : SID) : Boolean = exists(sid, RunSat())
    def isSat : Boolean = exists(RunSat())

    def getModel(sid : SID) : Option[Model] = {
      val (satSid, isEmpty) = SID.fromTopLevelSH(sh, sid).refineAndCheckEmptiness(RunSat())
      if (isEmpty) {
        println("Symbolic heap is unsatisfiable w.r.t. the given SID")
        None
      } else {
        satSid.witness.flatMap(_.getModel)
      }
    }

    def getModel : Option[Model] = {
      if (sh.predCalls.nonEmpty) throw new Throwable("Can't produce model of non-reduced heap without reference to an SID")
      Model.fromRSH(sh)
    }
  }

  class RichModel(model : Model) {
    def isModelOf(sh : SymbolicHeap) : Boolean = {
      if (sh.predCalls.nonEmpty) throw new Throwable("Can't model-check non-reduced heaps without reference to an SID")
      isModelOf(SID.fromTopLevelSH(sh, SID.empty("X")))
    }

    def isModelOf(sid : SID) : Boolean = GreedyUnfoldingModelChecker.isModel(model, sid)
  }

  implicit def ruleToHeap(rule : Rule) : SymbolicHeap = rule.body

  implicit def sidToRichSID(sid : SID) : RichSID = new RichSID(sid)

  implicit def sidToRichSH(sh : SymbolicHeap) : RichSymbolicHeap = new RichSymbolicHeap(sh)

  implicit def stringToInteractiveString(s : String) : ParsableString = new ParsableString(s)

  implicit def stringToSH(s : String) : RichSymbolicHeap = s.parse

  implicit def modelToRichModel(model : Model) : RichModel = new RichModel(model)

}
