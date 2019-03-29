package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PointsTo, PredCall, SymbolicHeap}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.util.Combinators

object GraphInterfaceAutomaton extends HeapAutomaton with HarrshLogging {

  override type State = GraphInterface

  override def isFinal(s: State): Boolean = false

  /**
    * Direct target computation
    */
  override def getTargetsFor(src: Seq[GraphInterface], lab: SymbolicHeap): Set[GraphInterface] = {
    logger.warn(s"Computing transition ${src.mkString(" * ")} -[$lab]--> ?")

    val closure = Closure.ofAtoms(lab.pure)
    val resultState = if (src.isEmpty && !lab.hasPointer) {
      GraphInterface(Set.empty, Set.empty, Set.empty, Set.empty, closure.classes.map(_.filter(_.isFree)))
    } else {
      interfaceOfNonEmptySh(src, lab, closure)
    }
    logger.warn(s"Result state: $resultState")
    Set(resultState)
  }

  private def interfaceOfNonEmptySh(src: Seq[GraphInterface], body: SymbolicHeap, closure: Closure): GraphInterface = {
    val callInterfaces = src.zip(body.predCalls).map(pair => callInterface(pair._2, pair._1))
    logger.warn("Instantiated interfaces: " + callInterfaces.mkString(" * "))
    val equivalenceClasses = mergeOverlappingClasses(closure.classes ++ callInterfaces.flatMap(_.equivalenceClasses))
    val (localAlloc, localRefed) = interfaceOfPto(body.pointers.toSet)
    val alloced = (localAlloc ++ callInterfaces.flatMap(_.alloced)).flatMap(classOf(_, equivalenceClasses))
    val refed = (localRefed ++ callInterfaces.flatMap(_.refed)).flatMap(classOf(_, equivalenceClasses)) -- alloced
    val (unmappedRoot, unmappedSink) = if (body.hasPointer) {
      (localAlloc, localRefed)
    } else {
      (callInterfaces.map(_.root).reduce(_.union(_)),
        callInterfaces.map(_.sink).reduce(_.union(_)))
    }
    val root = unmappedRoot.flatMap(classOf(_, equivalenceClasses))
    val sink = unmappedSink.flatMap(classOf(_, equivalenceClasses))
    logger.warn(s"Combined equivalence classes: ${equivalenceClasses.mkString(", ")} --> allocation $alloced, refed $refed, root $root, sink $sink")
    GraphInterface(alloced.filter(_.isFree), refed.filter(_.isFree), root.filter(_.isFree), sink.filter(_.isFreeNonNull), dropSingletons(equivalenceClasses.map(_.filter(_.isFree))))
  }

  private def dropSingletons(classes: Set[Set[Var]]): Set[Set[Var]] = classes.filter(_.size > 1)

  private def classOf(v: Var, classes: Set[Set[Var]]): Set[Var] = {
    classes.find(_.contains(v)).getOrElse(Set(v))
  }

  private def interfaceOfPto(pto: Set[PointsTo]): (Set[Var], Set[Var]) = {
    val alloced = pto.map(_.from)
    val refed = pto.flatMap(_.to) -- alloced
    (alloced, refed)
  }


  private def callInterface(call: PredCall, srcInterface: GraphInterface): GraphInterface = {
    srcInterface.rename(Var.getFvSeq(call.args.length), call.args)
  }
  
  case class GraphInterface(alloced: Set[Var],
                             refed: Set[Var],
                             root: Set[Var],
                             sink: Set[Var],
                             equivalenceClasses: Set[Set[Var]]) {

    def isEmptyGraph: Boolean = alloced.isEmpty && refed.isEmpty

    def rename(vars: Seq[Var], args: Seq[Var]): GraphInterface = {
      val map = vars.zip(args).toMap
      val rename = (v: Var) => map.getOrElse(v, v)
      GraphInterface(
        renameAll(alloced, rename),
        renameAll(refed, rename),
        renameAll(root, rename),
        renameAll(sink, rename),
        renameClasses(equivalenceClasses, rename)
      )
    }
  }

  object GraphInterface {
    def intersect(fst: GraphInterface, snd: GraphInterface) = GraphInterface(
      fst.alloced intersect snd.alloced,
      fst.refed intersect snd.refed,
      fst.root intersect snd.root,
      fst.sink intersect snd.sink,
      commonClasses(fst.equivalenceClasses, snd.equivalenceClasses)
    )
  }

  private def mergeOverlappingClasses(classes: Set[Set[Var]]): Set[Set[Var]] = {
    val allVars = classes.flatten
    var newClassesByVar = allVars.map{
      v => (v, classes.filter(_.contains(v)).flatten)
    }.toMap
    while (Combinators.counts(newClassesByVar.values.toSet.flatten).exists(_._2 > 1)) {
      val values = newClassesByVar.values.toSet
      newClassesByVar = allVars.map{
        v => (v, values.filter(_.contains(v)).flatten)
      }.toMap
    }
    newClassesByVar.values.toSet
  }

  private def renameClasses(equivalenceClasses: Set[Set[Var]], rename: Var => Var): Set[Set[Var]] = {
    dropSingletons(mergeOverlappingClasses(equivalenceClasses map (renameAll(_, rename))))
  }

  private def renameClasses(equivalenceClasses: Set[Set[Var]], vars: Seq[Var], args: Seq[Var]): Set[Set[Var]] = {
    val map = vars.zip(args).toMap
    val rename = (v: Var) => map.getOrElse(v, v)
    renameClasses(equivalenceClasses, rename)
  }

  private def renameAll[A](as: Set[A], f: A => A): Set[A] = {
    as map f
  }

  private def maybeIntersect[A](fst: Option[Set[A]], snd: Option[Set[A]]): Option[Set[A]] = {
    for {
      fstSet <- fst
      sndSet <- snd
    } yield fstSet intersect sndSet
  }

  private def commonClasses(fst: Set[Set[Var]], snd: Set[Set[Var]]): Set[Set[Var]] = {
    val allVars = fst.flatten ++ snd.flatten
    allVars.map{
      v => maybeIntersect(fst.find(_.contains(v)), snd.find(_.contains(v)))
    }.collect{
      case Some(intersection) => intersection
    }
  }

}
