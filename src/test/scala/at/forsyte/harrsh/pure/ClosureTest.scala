package at.forsyte.harrsh.pure

import at.forsyte.harrsh.TestValues
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/17/16.
  */
class ClosureTest extends HarrshTableTest with TestValues {

  // Test closure computation
  val fveqs = Table(
    ("pure", "results"),
    (Set[PureAtom](), Seq(true,true,true,true,true)),
    (Set[PureAtom](x1 =:= x2, x2 =:= x3), Seq(true, false, false, true, true)),
    (Set[PureAtom](x1 =:= x2, x2 =/= x3), Seq(true, false, true, true, true)),
    (Set[PureAtom](x1 =:= x2, x2 =:= x3, x4 =:= x5, x3 =:= x4), Seq(true,false,false,false,false)),
    (Set[PureAtom](x1 =:= x2, x2 =/= x3, x4 =:= x5, x3 =:= x4), Seq(true,false,true,false,false))
  )

  property("Congruence closure computation") {

    forAll(fveqs) {
      (eqs: Set[PureAtom], results: Seq[Boolean]) =>
        val closure = Closure.ofAtoms(eqs)

        for (i <- 1 to 5) {
          info(Var.defaultFV(i) + (if (results(i - 1)) " should be " else " should NOT be ") + "the minimal element in an equality class of " + eqs)
          closure.isRepresentative(Var.defaultFV(i)) should be(results(i - 1))
        }
    }

  }

}
