package slex.test.generators

import scala.collection.JavaConverters._

import org.scalacheck.Gen
import slex.Sorts.Location
import slex.models.{MapStack, Stack}

/**
  * Created by jkatelaa on 10/7/16.
  */
object StackGens {

  private def pairGen(vals : Seq[Location])(key : String) : Gen[(String, Location)] = Gen.oneOf(vals) map (v => (key,v))

  /**
    * Generator for stacks defined exactly on domain, each with a random value from vals, and on null, mapping to 0
    * @param domain Domain on which the generated stacks are defined
    * @param vals Possible values for each point of the domain (uniformly distributed)
    */
  def stackGen(domain : Set[String], vals : Set[Location]) : Gen[Stack] = {
    for {
      pairs <- Gen.sequence(domain map pairGen(vals.toSeq))
    } yield MapStack(Map[String, Location]("null" -> 0) ++ pairs.asScala)
  }

}
