package at.forsyte.harrsh.parsers.slcomp

import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentInstance
import at.forsyte.harrsh.parsers.EntailmentParsers

object ScriptToEntailmentInstance {

  def apply(s: Script, computeSeparateSidsForEachSide: Boolean): Option[EntailmentInstance] = {
    if (s.asserts.length != 2) {
      println(s"Excepted two asserts for entailment check, but received ${s.asserts.length}")
      None
    }
    else {
      val transformed = ScriptToBenchmark(s, "")
      transformed.toOption flatMap {
        res => EntailmentParsers.normalize(res, computeSeparateSidsForEachSide)
      }
    }
  }

}
