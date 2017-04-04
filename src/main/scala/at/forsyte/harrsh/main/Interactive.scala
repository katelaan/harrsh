package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.GreedyUnfoldingModelChecker
import at.forsyte.harrsh.main.interactive.AnnotatedResultBuffer
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

  // TODO Seq rather than set of rules in SID class

  private var paths = Seq("examples/datastructures", "examples/symbolicheaps")

  private val sidbuffer : AnnotatedResultBuffer[SID] = AnnotatedResultBuffer(10)
  private val shbuffer : AnnotatedResultBuffer[SymbolicHeap] = AnnotatedResultBuffer(20)
  //private var sidmap : Map[String,SID] = ???

  private var reportProgress = false

  def sid : SID = sid(1)
  def sids : Unit = println(sidbuffer.summarize)
  def sid(i : Int) : SID = sidbuffer(i)

  def heap : SymbolicHeap = heap(1)
  def heaps : Unit = println(shbuffer.summarize)
  def heap(i : Int) : SymbolicHeap = shbuffer(i)

  def clear : Unit = {
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
    val sat = RunSat()
    val unsat = RunUnsat()
    def mod(n : Int, d : Int) = RunModulo(n, d)
    val hasPtr = RunHasPointer()
    val establishment = RunEstablishment()
    val acyclic = RunWeakAcyclicity()
    val garbageFree = RunGarbageFreedom()
  }

  case class ParsableString(s : String) {
    def load() : Unit = {
      IOUtils.findFileIn(s, paths) match {
        case Some(file) =>
          println("Loading '" + file + "'")
          val sid = MainIO.getSidFromFile(file)
          sidbuffer.add(s, sid)
          println(sid + "  stored in variable 'sid'")
        case None =>
          println("Could not find file '" + s + "' in current path " + paths.mkString(":"))
      }
    }

    def parse() : Unit = {
      DefaultSIDParser.runOnSymbolicHeap(s) match {
        case Some(sh) =>
          shbuffer.add(s, sh)
          println(sh + "  stored in variable 'heap'")
        case None =>
          println("Could not parse '" + s + "' as symbolic heap")
      }

    }
  }

  case class RichSID(sid : SID) {
    def refineBy(task : AutomatonTask) : Unit = {
      RefinementAlgorithms.refineSID(sid, task.getAutomaton(sid.numFV), Duration(30, duration.SECONDS), reportProgress = reportProgress) match {
        case Some(refinedSID) =>
          println("Result of refinement: " + refinedSID._1)
          println("The resulting SID is " + (if (refinedSID._2) "empty" else "nonempty"))
          sidbuffer.add("Refinement by " + task, refinedSID._1)
          println("Result stored in variable 'sid'")
        case None =>
          println("Refinement failed")
      }
    }

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

    def simplify = ???

    def isA(sid : SID) : Unit = {
      println("Checking " + sh + " |= " + sid.callToStartPred)
      println("Result: " + GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(sh, sid.callToStartPred, sid, reportProgress))
    }
  }

  implicit def ruleToHeap(rule : Rule) : SymbolicHeap = rule.body

  implicit def sidToRichSID(sid : SID) : RichSID = RichSID(sid)

  implicit def sidToRichSH(sh : SymbolicHeap) : RichSymbolicHeap = RichSymbolicHeap(sh)

  implicit def stringToInteractiveString(s : String) : ParsableString = ParsableString(s)

  object examples {

    def sll: Unit = {
      clear
      "sll.sid".load
      "x1 -> y1 * y1 -> y2 * y2 -> x2".parse
      "sll(x1, y1) * sll(y1, x2)".parse
    }

    def tll: Unit = {
      clear
      "tll.sid".load
      record("TLL call", sid.callToStartPred)
    }

  }

}
