package at.forsyte.harrsh.pure

import at.forsyte.harrsh.pure.EqualityUtils.mkPure
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/17/16.
  */
class ClosureTest extends HarrshTableTest {

  // Test closure computation
  val fveqs = Table(
    ("pure", "results"),
    (mkPure(), Seq(true,true,true,true,true)),
    (mkPure((1,2,true), (2,3,true)), Seq(true, false, false, true, true)),
    (mkPure((1,2,true), (2,3,false)), Seq(true, false, true, true, true)),
    (mkPure((1,2,true), (2,3,true), (4,5,true), (3,4,true)), Seq(true,false,false,false,false)),
    (mkPure((1,2,true), (2,3,false), (4,5,true), (3,4,true)), Seq(true,false,true,false,false))
  )

  property("Congruence closure computation") {

    forAll(fveqs) {
      (eqs: Set[PureAtom], results: Seq[Boolean]) =>
        val closure = Closure.ofSetOfAtoms(eqs)

        for (i <- 1 to 5) {
          info(Var(i) + (if (results(i - 1)) " should be " else " should NOT be ") + "the minimal element in an equality class of " + eqs)
          closure.isRepresentative(Var(i)) should be(results(i - 1))
        }
    }

  }

}
