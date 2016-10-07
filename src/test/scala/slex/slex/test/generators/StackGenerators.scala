package slex.slex.test.generators

import org.scalacheck.Gen
import slex.Sorts.Location
import slex.models.{MapStack, Stack}

/**
  * Created by jkatelaa on 10/7/16.
  */
object StackGenerators {

  private def pairGen(vals : Seq[Location])(key : String) : Gen[(String, Location)] =
    for {
      v <- Gen.oneOf(vals)
    } yield(key, v)

  def stackGen(domain : Seq[String], vals : Set[Location]) : Gen[Stack] = {
//    for {
//      pairs : Seq[(String,Location)] <- Gen.sequence(domain map (pairGen(vals.toSeq)_))
//    } yield new MapStack(Map[String,Location]() ++ pairs)
    ???
  }

}
