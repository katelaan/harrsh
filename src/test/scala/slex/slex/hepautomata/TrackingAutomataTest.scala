package slex.slex.hepautomata

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import slex.SlexTest
import slex.heapautomata.TrackingAutomata
import slex.heapautomata.TrackingAutomata.FVEquality

/**
  * Created by jens on 10/16/16.
  */
class TrackingAutomataTest extends SlexTest with TableDrivenPropertyChecks {

  println("Checking congruence closure computation")

  // Test closure computation
  val fveqs = Table(
    ("pure", "results"),
    (Set[FVEquality](), Seq(true,true,true,true,true)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,true)), Seq(true, false, false, true, true)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,false)), Seq(true, false, true, true, true)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,true), FVEquality(4,5,true), FVEquality(3,4,true)), Seq(true,false,false,false,false)),
    (Set(FVEquality(1,2,true), FVEquality(2,3,false), FVEquality(4,5,true), FVEquality(3,4,true)), Seq(true,false,true,false,false))
  )

  forAll(fveqs) {
    (eqs : Set[FVEquality], results : Seq[Boolean]) =>
      val isRep = TrackingAutomata.computeRepresentationsInClosure(eqs)

      for (i <- 1 to 5) {
        println("Representation of " + eqs + " applied to " + i + " should yield " + results(i-1))
        isRep(i) should be(results(i - 1))
      }
  }



  println("Testing emptiness for the Tracking automaton")

}
