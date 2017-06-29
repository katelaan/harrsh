package at.forsyte.harrsh.test

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jkatelaa on 10/7/16.
  */
abstract class HarrshTest extends FlatSpec with Matchers {

  /**
    * Skip the given computation (to temporarily deactivate individual test cases)
    * @param comp Computation to skip
    */
  def skip(comp : => Unit) = ()

}
