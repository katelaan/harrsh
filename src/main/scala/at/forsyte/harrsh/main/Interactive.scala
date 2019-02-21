package at.forsyte.harrsh.main

import at.forsyte.harrsh.Defaults
import at.forsyte.harrsh.Implicits.{ParsableString, RichModel, RichSID, RichSymbolicHeap}
import at.forsyte.harrsh.modelchecking.Model
import at.forsyte.harrsh.main.interactive.AnnotatedResultBuffer
import at.forsyte.harrsh.refinement._
import at.forsyte.harrsh.seplog.inductive.{RuleBody, Sid, SymbolicHeap}
import at.forsyte.harrsh.util.IOUtils

import scala.concurrent.duration
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

/**
  * Created by jens on 4/3/17.
  */
object Interactive {

  private val sidbuffer : AnnotatedResultBuffer[Sid] = AnnotatedResultBuffer(10)
  private val shbuffer : AnnotatedResultBuffer[SymbolicHeap] = AnnotatedResultBuffer(20)
  private var loadedSids : Map[String,Sid] = Map.empty

  private val InteractiveTimeout = Duration(30, duration.SECONDS)

  def sid : Sid = sids(1)
  def sids() : Unit = println(sidbuffer.summarize)
  def sids(i : Int) : Sid = sidbuffer(i)

  def heap : SymbolicHeap = heaps(1)
  def heaps() : Unit = println(shbuffer.summarize)
  def heaps(i : Int) : SymbolicHeap = shbuffer(i)

  def clear() : Unit = {
    sidbuffer.clear
    shbuffer.clear
  }

  // TODO Useful explain-why summaries
  /*def explainWhy(f : => Unit) : Unit = {
    Defaults.reportProgress = true
    f
    Defaults.reportProgress = false
  }*/

  def record(desc : String, sh : => SymbolicHeap) : SymbolicHeap = {
    val eval : SymbolicHeap = sh
    shbuffer.add(desc, eval)
    //println("Storing result in variable 'heap'")
    eval
  }

//  def record(desc : String, shs : => Iterable[SymbolicHeap]) : Iterable[SymbolicHeap] = {
//    shbuffer.addAll(desc, shs)
//    println("Storing result in buffer 'heaps'")
//    shs
//  }

  object automata {
    val hasPtr = RunHasPointer()
    def odd = RunModulo(1,2)
    def even = RunModulo(0,2)
    def mod(n : Int, d : Int) = RunModulo(n, d)
    val sat = RunSat
    val unsat = RunUnsat
    val establishment = RunEstablishment
    val nonEstablishment = RunNonEstablishment
    val acyclic = RunWeakAcyclicity
    val cyclic = RunStrongCyclicity
    val garbageFree = RunGarbageFreedom
    val hasGarbage = RunMayHaveGarbage
  }

  case class BufferingParsableString(override val s : String) extends ParsableString(s) {

    override def load() : Sid = {
      println("Loading '" + s + "'")
      val sid = super.load()
      sidbuffer.add(s, sid)
      loadedSids = loadedSids + (sid.startPred -> sid)
      //println("Result stored in variable 'sid'")
      sid
    }

    override def parse : SymbolicHeap = {
      record(s, super.parse)
    }

    def get() : Sid = loadedSids(s)
  }

  case class BufferingRichSID(override val sid : Sid) extends RichSID(sid) {

    override def refineAndCheckEmptiness(task : AutomatonTask) : (Sid,Boolean) = {
      val res@(refinedSID,isEmpty) = super.refineAndCheckEmptiness(task)
      sidbuffer.add("Refinement by " + task, refinedSID)
      //println("Result stored in variable 'sid'")
      //println("The resulting SID is " + (if (isEmpty) "empty" else "nonempty"))
      res
    }

    override def witness : Option[SymbolicHeap] = super.witness map (record("witness", _))

    def analyze : Unit = {
      RefinementAlgorithms.performFullAnalysis(sid, InteractiveTimeout, verbose = false)
    }
  }

  case class BufferingRichSymbolicHeap(override val sh : SymbolicHeap) extends RichSymbolicHeap(sh) {

    override def unfoldFirstCall(by : SymbolicHeap) : SymbolicHeap = record("unfold 1st", super.unfoldFirstCall(by))
    override def unfoldSecondCall(by : SymbolicHeap) : SymbolicHeap = record("unfold 2nd", super.unfoldSecondCall(by))
    override def unfoldIthCall(i : Int, by : SymbolicHeap) : SymbolicHeap = record("unfold " + i + "th", super.unfoldIthCall(i, by))
    override def simplify : SymbolicHeap = record("simplified", super.simplify)

    override def isA(sid: Sid): Boolean = {
      println("Checking " + sh + " |= " + sid.callToStartPred)
      super.isA(sid)
    }

  }

  implicit def ruleToHeap(rule : RuleBody) : SymbolicHeap = rule.body

  implicit def sidToRichSID(sid : Sid) : BufferingRichSID = BufferingRichSID(sid)

  implicit def sidToRichSH(sh : SymbolicHeap) : BufferingRichSymbolicHeap = BufferingRichSymbolicHeap(sh)

  implicit def stringToInteractiveString(s : String) : BufferingParsableString = BufferingParsableString(s)

  implicit def stringToSH(s : String) : BufferingRichSymbolicHeap = s.parse

  implicit def stringToSID(s : String) : BufferingRichSID = loadedSids(s)

  implicit def modelToRichModel(model : Model) : RichModel = new RichModel(model)

  object examples {

    def dataStructures() : Unit = {
      "sll.sid".load()
      "dll.sid".load()
      "tree.sid".load()
      "tll.sid".load()
    }

  }

  case class RichIterable[A](it : Iterable[A]) {
    def onePerLine : String = it.mkString("\n")
  }

  implicit def iterableToRichIterable[A](it : Iterable[A]) : RichIterable[A] = RichIterable(it)

  def listFilesInPath(substring : String) : Unit = {
    val matchingFiles = IOUtils.listOfFilesIn(Defaults.PathsToExamples).filter(_.getAbsolutePath.contains(substring))
    println(matchingFiles.mkString("\n"))
  }

}
