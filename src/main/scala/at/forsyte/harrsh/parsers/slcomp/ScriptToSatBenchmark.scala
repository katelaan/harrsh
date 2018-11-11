package at.forsyte.harrsh.parsers.slcomp

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog._

object ScriptToSatBenchmark extends HarrshLogging {

  def apply(s: Script, description: String): SatBenchmark = {
    if (s.asserts.length != 1) {
      throw new IllegalArgumentException(s"Can only build top-level symbolic heap from 1 assert, but received ${s.asserts.length}")
    }

    val transformed = ScriptToBenchmark(s, description)
    transformed match {
      case Left(value) => value
      case Right(value) => throw new IllegalArgumentException("Instance could only be parsed as entailment instance")
    }
  }
}
