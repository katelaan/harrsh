package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.GreedyUnfoldingModelChecker
import at.forsyte.harrsh.main.interactive.AnnotatedResultBuffer
import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.refinement.DecisionProcedures.AnalysisResult
import at.forsyte.harrsh.refinement._
import at.forsyte.harrsh.seplog.inductive.{Rule, SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.seplog.parsers.DefaultSIDParser
import at.forsyte.harrsh.util.IOUtils

import scala.concurrent.duration
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.language.postfixOps

/**
  * Created by jens on 4/3/17.
  */
object Interactive {

  // TODO Parser for pretty-printed SHs
  // TODO Better explain-why summaries

  private var paths = Seq("examples/datastructures", "examples/symbolicheaps")

  private val sidbuffer : AnnotatedResultBuffer[SID] = AnnotatedResultBuffer(10)
  private val shbuffer : AnnotatedResultBuffer[SymbolicHeap] = AnnotatedResultBuffer(20)
  private var loadedSids : Map[String,SID] = Map.empty

  private var reportProgress = false

  private val InteractiveTimeout = Duration(30, duration.SECONDS)

  def sid : SID = sids(1)
  def sids() : Unit = println(sidbuffer.summarize)
  def sids(i : Int) : SID = sidbuffer(i)

  def heap : SymbolicHeap = heaps(1)
  def heaps() : Unit = println(shbuffer.summarize)
  def heaps(i : Int) : SymbolicHeap = shbuffer(i)

  def clear() : Unit = {
    sidbuffer.clear
    shbuffer.clear
  }

  def explainWhy(f : => Unit) : Unit = {
    reportProgress = true
    f
    reportProgress = false
  }

  def record(desc : String, sh : => SymbolicHeap) : SymbolicHeap = {
    shbuffer.add(desc, sh)
    println("Storing result in variable 'heap'")
    sh
  }

  def record(desc : String, shs : => Iterable[SymbolicHeap]) : Iterable[SymbolicHeap] = {
    shbuffer.addAll(desc, shs)
    println("Storing result in buffer 'heaps'")
    shs
  }

  object automata {
    val hasPtr = RunHasPointer()
    def mod(n : Int, d : Int) = RunModulo(n, d)
    val sat = RunSat()
    val unsat = RunUnsat()
    val establishment = RunEstablishment()
    val nonEstablishment = RunNonEstablishment()
    val acyclic = RunWeakAcyclicity()
    val cyclic = RunStrongCyclicity()
    val garbageFree = RunGarbageFreedom()
    val hasGarbage = RunMayHaveGarbage()
  }

  case class ParsableString(s : String) {

    def load() : SID = {
      IOUtils.findFileIn(s, paths) match {
        case Some(file) =>
          println("Loading '" + file + "'")
          val sid = MainIO.getSidFromFile(file)
          sidbuffer.add(s, sid)
          loadedSids = loadedSids + (sid.startPred -> sid)
          println("Result stored in variable 'sid'")
          sid
        case None =>
          println("Could not find file '" + s + "' in current path " + paths.mkString(":"))
          SID.empty("fail")
      }
    }

    def parse() : SymbolicHeap = {
      DefaultSIDParser.runOnSymbolicHeap(s) match {
        case Some(sh) =>
          record(s, sh)
        case None =>
          println("Could not parse '" + s + "' as symbolic heap")
          SymbolicHeap.empty
      }
    }

    def get() : SID = loadedSids(s)
  }

  case class RichSID(sid : SID) {

    def refineBy(task : AutomatonTask) : SID = {
      RefinementAlgorithms.refineSID(sid, task.getAutomaton(sid.numFV), InteractiveTimeout, reportProgress = reportProgress) match {
        case Some(refinedSID) =>
          sidbuffer.add("Refinement by " + task, refinedSID._1)
          println("Result stored in variable 'sid'")
          println("The resulting SID is " + (if (refinedSID._2) "empty" else "nonempty"))
          refinedSID._1
        case None =>
          println("Refinement failed")
          SID.empty(sid.startPred)
      }
    }

    def decide(task : AutomatonTask) : Boolean = {
      val AnalysisResult(isEmpty, analysisTime, timedOut) = DecisionProcedures.decideInstance(sid, task.getAutomaton(sid.numFV), InteractiveTimeout, verbose = reportProgress, reportProgress = reportProgress)
      if (timedOut) {
        println("Reached timeout of " + InteractiveTimeout)
      } else {
        println("Finished analysis in " + analysisTime + "ms")
      }
      !isEmpty
    }

    def witness : SymbolicHeap = SIDUnfolding.firstReducedUnfolding(sid)

    def baseRule : Rule = {
      val base = sid.rules.filter(!_.body.hasPredCalls)
      if (base.size > 1) {
        println("Warning: More than one base rule. Will pick arbitrary one")
      }
      base.head
    }

    def recursiveRule : Rule = {
      val rec = sid.rules.filter(_.body.hasPredCalls)
      if (rec.size > 1) {
        println("Warning: More than one recursive rule. Will pick arbitrary one")
      }
      rec.head
    }
  }

  case class RichSymbolicHeap(sh : SymbolicHeap) {

    def unfoldFirstCall(by : SymbolicHeap) : SymbolicHeap = record("unfold 1st", sh.replaceCall(sh.predCalls.head, by))
    def unfoldSecondCall(by : SymbolicHeap) : SymbolicHeap = record("unfold 2nd", sh.replaceCall(sh.predCalls(1), by))
    def unfoldIthCall(i : Int, by : SymbolicHeap) : SymbolicHeap = record("unfold " + i + "th", sh.replaceCall(sh.predCalls(i-1), by))
    def unfoldOnce(sid : SID) : Iterable[SymbolicHeap] = record("unfolding", SIDUnfolding.unfoldOnce(sid, Seq(sh)))
    def unfoldings(sid : SID, depth : Int) : Iterable[SymbolicHeap] = SIDUnfolding.unfold(sid, depth, false)
    def reducedUnfoldings(sid : SID, depth : Int) : Iterable[SymbolicHeap] = SIDUnfolding.unfold(sid, depth, true)

    def simplify : SymbolicHeap = EqualityBasedSimplifications.removeExplicitlyRedundantBoundVars(sh)

    def isA(sid : SID) : Boolean = {
      println("Checking " + sh + " |= " + sid.callToStartPred)
      //println("Result: " + )
      GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(sh, sid.callToStartPred, sid, reportProgress)
    }

    def refineBy(sid: SID, task : AutomatonTask) : SID = {
      SID.fromTopLevelSH(sh, sid).refineBy(task)
    }

    def decide(sid: SID, task : AutomatonTask) : Boolean = {
      SID.fromTopLevelSH(sh, sid).decide(task)
    }
  }

  implicit def ruleToHeap(rule : Rule) : SymbolicHeap = rule.body

  implicit def sidToRichSID(sid : SID) : RichSID = RichSID(sid)

  implicit def sidToRichSH(sh : SymbolicHeap) : RichSymbolicHeap = RichSymbolicHeap(sh)

  implicit def stringToInteractiveString(s : String) : ParsableString = ParsableString(s)

  implicit def stringToSH(s : String) : RichSymbolicHeap = s.parse

  implicit def stringToSID(s : String) : RichSID = loadedSids(s)

  object examples {

    def dataStructures() : Unit = {
      "sll.sid".load()
      "dll.sid".load()
      "tree.sid".load()
      "tll.sid".load()
    }

    def sll(): Unit = {
      clear()
      "sll.sid".load()
      "x1 -> y1 * y1 -> y2 * y2 -> x2".parse()
      "sll(x1, y1) * sll(y1, x2)".parse()
    }

    def tll(): Unit = {
      clear()
      "tll.sid".load()
      record("TLL call", sid.callToStartPred)
    }

  }

}
