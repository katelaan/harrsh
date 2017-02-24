package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{NullPtr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PointsTo, PtrEq, SymbolicHeap}
import at.forsyte.harrsh.test.{HarrshTableTest, HarrshTest}

/**
  * Created by jens on 2/24/17.
  */
class ModelCheckingTest extends HarrshTableTest {

  // TODO Add test cases violating the assertions

  val testCases = Table(
    ("stack", "heap", "result"),
    // Simple model with pairwise different variables, all of which are non-null
    (Map() + (1 -> 5, 2 -> 42, 3 -> 55),
      Map() + (5 -> Seq(42, 55), 42 -> Seq(55), 55 -> Seq(3), 3 -> Seq(0)),
      SymbolicHeap(List(),List(PointsTo(PtrVar(1),List(PtrVar(2), PtrVar(3))), PointsTo(PtrVar(2),List(PtrVar(3))), PointsTo(PtrVar(3),List(PtrVar(-1))), PointsTo(PtrVar(-1),List(NullPtr()))),3,List(-1))),

    // Stack with some null vars
      (Map() + (1 -> 5, 2 -> 42, 3 -> 0, 4 -> 0),
       Map() + (5 -> Seq(42, 55), 42 -> Seq(55), 55 -> Seq(3), 3 -> Seq(0)),
       SymbolicHeap(List(PtrEq(PtrVar(3),PtrVar(4)), PtrEq(PtrVar(3),NullPtr()), PtrEq(PtrVar(4),NullPtr())),List(PointsTo(PtrVar(1),List(PtrVar(2), PtrVar(-1))), PointsTo(PtrVar(2),List(PtrVar(-1))), PointsTo(PtrVar(-1),List(PtrVar(-2))), PointsTo(PtrVar(-2),List(NullPtr()))),4,List(-1, -2))),

    // Additionally multiple equal vars
    (Map() + (1 -> 5, 2 -> 42, 3 -> 0, 4 -> 0, 5 -> 42, 6 -> 42),
      Map() + (5 -> Seq(42, 55), 42 -> Seq(55), 55 -> Seq(3), 3 -> Seq(0)),
      SymbolicHeap(List(PtrEq(PtrVar(5),PtrVar(6)), PtrEq(PtrVar(6),PtrVar(2)), PtrEq(PtrVar(3),PtrVar(4)), PtrEq(PtrVar(3),NullPtr()), PtrEq(PtrVar(4),NullPtr())),List(PointsTo(PtrVar(1),List(PtrVar(2), PtrVar(-1))), PointsTo(PtrVar(2),List(PtrVar(-1))), PointsTo(PtrVar(-1),List(PtrVar(-2))), PointsTo(PtrVar(-2),List(NullPtr()))),6,List(-1, -2))),

    //Stack with multiple bound vars
    (Map() + (1 -> 5, 2 -> 42, 3 -> 0),
      Map() + (5 -> Seq(42, 55), 42 -> Seq(55), 55 -> Seq(3,8), 3 -> Seq(0), 8 -> Seq(19, 22), 19 -> Seq(0), 22 -> Seq(0)),
      SymbolicHeap(List(PtrEq(PtrVar(3),NullPtr())),List(PointsTo(PtrVar(1),List(PtrVar(2), PtrVar(-1))), PointsTo(PtrVar(2),List(PtrVar(-1))), PointsTo(PtrVar(-2),List(NullPtr())), PointsTo(PtrVar(-3),List(NullPtr())), PointsTo(PtrVar(-1),List(PtrVar(-3), PtrVar(-4))), PointsTo(PtrVar(-4),List(PtrVar(-5), PtrVar(-2))), PointsTo(PtrVar(-5),List(NullPtr()))),3,List(-1, -2, -3, -4, -5)))
  )

  property("The conversion of models to symbolic heaps") {
    forAll(testCases) {
      (stack : Map[Var,Loc], heap : Map[Loc,Seq[Loc]], expectedRes : SymbolicHeap) =>
        val res = ModelChecking.modelToFormula(Model(stack, heap))
        res shouldEqual expectedRes
    }
  }

}
