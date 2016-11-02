package at.forsyte.harrsh.hepautomata

import at.forsyte.harrsh.main.FV._
import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.heapautomata.utils.ClosureOfAtomSet
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
        val closure = new ClosureOfAtomSet(eqs)

        for (i <- 1 to 5) {
          info(fv(i) + (if (results(i - 1)) " should be " else " should NOT be ") + "the minimal element in an equality class of " + eqs)
          closure.isMinimumInItsClass(fv(i)) should be(results(i - 1))
        }
    }

  }

}
