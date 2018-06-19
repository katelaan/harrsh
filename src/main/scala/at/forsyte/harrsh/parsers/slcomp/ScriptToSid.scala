package at.forsyte.harrsh.parsers.slcomp

import at.forsyte.harrsh.seplog.inductive.SID

object ScriptToSid {

  def apply(s: Script): SID = {
    println(s"Will translate the following script:\n$s")
    SID.empty
  }

}
