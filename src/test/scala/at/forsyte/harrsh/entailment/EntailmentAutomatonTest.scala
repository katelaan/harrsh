package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.test.HarrshTableTest
import at.forsyte.harrsh.{ExampleSids, TestValues}
import org.scalatest.prop.TableFor4

class EntailmentAutomatonTest extends HarrshTableTest with TestValues {

  import EntailmentAutomatonTest._

  property("Soundness of entailment for singly-linked lists") {

    val nel = ExampleSids.Nel
    val odd = ExampleSids.OddNel
    val even = ExampleSids.EvenNel
    val anel = ExampleSids.AcycNel
    val sinlgePtrLhs = SidFactory.fromSymbolicHeap(SymbolicHeap(x1 -> x2))
    val reversedSinlgePtrLhs = SidFactory.fromSymbolicHeap(SymbolicHeap(x2 -> x1))
    val twoPtrLhs = SidFactory.makeRootedSid("twoptrs",
      "List of length 2",
      Map("twoptrs" -> x1),
      // twoptrs <= emp : { a = b }
      ("oneptr", Seq.empty, SymbolicHeap(x1 -> x2)),
      // twoptrs <= ∃ y . a -> y * twoptrs(y, b)
      ("twoptrs", Seq("y"), SymbolicHeap(x1 -> y1, P("oneptr")(y1, x2)))
    )
    val twoFields = SidFactory.fromSymbolicHeap(SymbolicHeap(x1 -> (x2,nil)))
    val oneOrTwoFields = SidFactory.makeRootedSid("onetwo",
      "Either a list pointer or a list pointer with an extra field",
      Map("onetwo" -> x1),
      ("onetwo", Seq.empty, SymbolicHeap(x1 -> x2)),
      ("onetwo", Seq.empty, SymbolicHeap(x1 -> (x2,nil)))
    )
    val oddeven = SidFactory.makeRootedSid("oddeven",
      "Nonempty lists of odd length",
      Map("oddeven" -> x1, "odd" -> x1, "even" -> x1),
      ("oddeven", Seq("n"), SymbolicHeap(x1 -> x2)),
      ("oddeven", Seq("n"), SymbolicHeap(x1 -> y1, P("even")(y1, x2))),
      ("oddeven", Seq("n"), SymbolicHeap(x1 -> y1, P("odd")(y1, x2))),
      ("odd", Seq("n"), SymbolicHeap(x1 -> x2)),
      ("odd", Seq("n"), SymbolicHeap(x1 -> y1, P("even")(y1, x2))),
      ("even", Seq("n"), SymbolicHeap(x1 -> y1, P("odd")(y1, x2))))

    val sllTable = Table(
      ("lhsSid", "rhsSid", "rhsCall", "shouldHold"),
      // x1 -> x2 |= nel(x1,x2)
      (sinlgePtrLhs, nel, P("nel")(x1,x2), EntailmentHolds),
      // x1 -> x2 |/= nel(z1,z2)
      (sinlgePtrLhs, nel, P("nel")(FreeVar("z1"),FreeVar("z2")), EntailmentFails),
      // x2 -> x1 |/= nel(x1,x2)
      (reversedSinlgePtrLhs, nel, P("nel")(x1,x2), EntailmentFails),
      // x2 -> x1 |= nel(x2,x1)
      (reversedSinlgePtrLhs, nel, P("nel")(x2,x1), EntailmentHolds),
      // ex. y . x1 -> y * y -> x2 |= nel(x1,x2)
      (twoPtrLhs, nel, P("nel")(x1,x2), EntailmentHolds),
      // ex. y . x1 -> y * y -> x2 |= nel(x1,x2)
      (twoPtrLhs, nel, P("nel")(x2,x1), EntailmentFails),
      // x1 -> (x2, nil) |/= nel(x1, x2)
      (twoFields, nel, P("nel")(x1,x2), EntailmentFails),
      // x1 -> x2 \/ x1 -> (x2, nil) |/= nel(x1, x2)
      (oneOrTwoFields, nel, P("nel")(x1,x2), EntailmentFails),
      // Every list is a list
      (nel, nel, P("nel")(x1,x2), EntailmentHolds),
      // Every odd list is a list
      (odd, nel, P("nel")(x1,x2), EntailmentHolds),
      // Not every list is an odd list
      (nel, odd, P("odd")(x1,x2), EntailmentFails),
      // Every even list is a list
      (even, nel, P("nel")(x1,x2), EntailmentHolds),
      // Not every list is an even list
      (nel, even, P("even")(x1,x2), EntailmentFails),
      // Every list that's odd or even in length is a list
      (oddeven, nel, P("nel")(x1,x2), EntailmentHolds),
      // Every list is a list that's odd or even in length
      (nel, oddeven, P("oddeven")(x1,x2), EntailmentHolds),
      // Every acyclic list is a list
      (anel, nel, P("nel")(x1,x2), EntailmentHolds),
      // Not every list is an acyclic list
      (nel, anel, P("anel")(x1,x2), EntailmentFails)
    )

    runAllTestsInTable(sllTable)

  }

  property("Soundness of entailment for doubly-linked lists") {

    val dll = ExampleSids.NeDll

    val dllTable = Table(
      ("lhsSid", "rhsSid", "rhsCall", "shouldHold"),
      // Every tree is a tree
      (dll, dll, P("dll")(x1, x2, x3), EntailmentHolds)
    )

    runAllTestsInTable(dllTable)

  }

  property("Soundness of entailment for singly-linked trees") {

    val tree = ExampleSids.Tree
    val singleTreePtr = SidFactory.fromSymbolicHeap(SymbolicHeap(x1 -> (nil,nil)))
    val almostLinearTree = SidFactory.makeRootedSid("ltree",
      "Null-terminated tree",
      Map("ltree" -> x1, "rtree" -> x1),
      ("ltree", Seq.empty,  SymbolicHeap(x1 -> (nil, nil))),
      ("ltree", Seq("y", "z"), SymbolicHeap(x1 -> (y1, y2), P("ltree")(y1), P("rtree")(y2))),
      ("rtree", Seq.empty, SymbolicHeap(x1 -> (nil, nil)))
    )
    val singleTreePtrWoNullInfo = SidFactory.fromSymbolicHeap(SymbolicHeap(x1 -> (x2,x3)))
    val singleTreePtrWithNullInfo = SidFactory.fromSymbolicHeap(SymbolicHeap(x1 -> (x2,x3), x2 =:= nil, x3 =:= nil))

    val treeTable = Table(
      ("lhsSid", "rhsSid", "rhsCall", "shouldHold"),
      // Every tree is a tree
      (tree, tree, P("tree")(x1), EntailmentHolds),
      (singleTreePtr, tree, P("tree")(x1), EntailmentHolds),
      // Every almost-linear tree is a tree
      (almostLinearTree, tree, P("tree")(x1), EntailmentHolds),
      // Not every tree is an almost-linear tree
      (tree, almostLinearTree, P("ltree")(x1), EntailmentFails),
      (singleTreePtrWoNullInfo, tree, P("tree")(x1), EntailmentFails),
      (singleTreePtrWithNullInfo, tree, P("tree")(x1), EntailmentHolds)
    )

    runAllTestsInTable(treeTable)

  }

  property("Soundness of entailment for TLLs") {

    val tll = ExampleSids.Tll
    val tllAcyc = ExampleSids.TllAcyc

    val tllTable = Table(
      ("lhsSid", "rhsSid", "rhsCall", "shouldHold"),
      (tll, tll, P("tll")(x1,x2,x3), EntailmentHolds),
      (tllAcyc, tll, P("tll")(x1,x2,x3), EntailmentHolds),
      (tll, tllAcyc, P("tll")(x1,x2,x3), EntailmentFails)
    )

    runAllTestsInTable(tllTable)

  }

  private def runAllTestsInTable[A <: SidLike](table: TableFor4[A, RichSid, PredCall, Boolean]) = {
    forAll(table) {
      (lhsSid, rhsSid, rhsCall, shouldHold) =>
        Given(s"LHS $lhsSid and RHS $rhsCall w.r.t. RHS-SID $rhsSid")
        Then(s"Entailment should hold: $shouldHold")

        val (aut, reach) = refine(rhsSid, rhsCall, lhsSid)
        info("Refinement result: " + EntailmentChecker.serializeFixedPoint(aut, reach))
        verifyEntailment(aut, lhsSid.startPred, reach) shouldEqual shouldHold
    }
  }


}

object EntailmentAutomatonTest extends TestValues {

  val EntailmentFails = false
  val EntailmentHolds = true

  def main(args: Array[String]): Unit = {

    // Limitation of current implementation: Can't deal with redundant extra vars on LHS
    val tree = ExampleSids.Tree
    val singleTreePtrWithNullInfo = SidFactory.fromSymbolicHeap(SymbolicHeap(x1 -> (x2,x3), x2 =:= nil, x3 =:= nil))
    val (lhsSid, rhsSid, rhsCall, shouldHold) = (singleTreePtrWithNullInfo, tree, P("tree")(x1), EntailmentHolds)

    println(s"Success: " + (check(rhsSid, rhsCall, lhsSid) == shouldHold))
  }

  def refine(sid: RichSid, rhs: PredCall, lhs: SidLike): (EntailmentAutomaton, Map[String, Set[EntailmentProfile]]) = {
    val aut = new EntailmentAutomaton(sid, TopLevelConstraint(Seq(rhs), Seq.empty))
    val reachable = RefinementAlgorithms.allReachableStates(lhs, aut, reportProgress = true)
    (aut, reachable)
  }

  def verifyEntailment(aut: EntailmentAutomaton, lhsTopLevelPred: String, reachable: Map[String, Set[EntailmentProfile]]) = {
    val isFinal = (s: EntailmentProfile) => aut.isFinal(s)
    if (!reachable.keySet.contains(lhsTopLevelPred)) {
      throw new IllegalArgumentException(s"Malformed test case: LHS start predicate $lhsTopLevelPred unreachable, so entailment trivially holds")
    }
    val entailmentHolds = reachable(lhsTopLevelPred).forall(isFinal)
    entailmentHolds
  }

  def check(sid: RichSid, rhs: PredCall, lhs: SidLike): Boolean = {
    println(s"Checking ${lhs.callToStartPred} |= $rhs for SID '${sid.description}'")
    val (aut, reachable) = refine(sid, rhs, lhs)
    println(EntailmentChecker.serializeFixedPoint(aut, reachable))
    verifyEntailment(aut, lhs.startPred, reachable)
  }



}
