package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.test.HarrshTableTest
import at.forsyte.harrsh.{ExampleSids, Implicits, TestValues}

class SplitIntoRootedComponentsTest extends HarrshTableTest with Implicits with TestValues {

  val nelSid = ExampleSids.Nel

  val inputs = Table(
    ("symbolic heap", "sid", "reachability components"),
    ("x1 -> x2".parse, nelSid, List("x1 -> x2".parse)),
    ("x1 -> x2 * x2 -> x3".parse, nelSid, List("x1 -> x2 * x2 -> x3".parse)),
    ("x1 -> y1 * y1 -> x2".parse, nelSid, List("x1 -> y1 * y1 -> x2".parse)),
    ("x1 -> x2 * x3 -> x4".parse, nelSid, List("x1 -> x2".parse, "x3 -> x4".parse)),
    ("x1 -> x2 * x3 -> x4 : {x2 = x3}".parse, nelSid, List("x1 -> x2 * x3 -> x4 : {x2 = x3}".parse)),
    ("x1 -> x2 * nel(x2, x3) * x3 -> x4".parse, nelSid, List("x1 -> x2 * nel(x2, x3) * x3 -> x4".parse)),
    ("x1 -> x2 * nel(x2, x3) * x3 -> null * nel(x4, x5) * x5 -> x6 * nel(x6, null)".parse, nelSid, List("x1 -> x2 * nel(x2, x3) * x3 -> null".parse, "nel(x4, x5) * x5 -> x6 * nel(x6, null)".parse)),
    ("x1 -> (x2, x3) * x2 -> x4 * x5 -> x6 : {x3 = x5}".parse, nelSid, List("x1 -> (x2, x3) * x2 -> x4 * x5 -> x6 : {x3 = x5}".parse)),
    ("x1 -> (x2, x3) * x2 -> x4 * x5 -> x6 : {x3 != x5}".parse, nelSid, List("x1 -> (x2, x3) * x2 -> x4 : {x3 != x5}".parse, "x5 -> x6".parse))
  )

  property("Splitting symbolic heps into reachability components") {
    forAll(inputs) {
      (sh, sid, expectedResult) =>
        info(s"Will split $sh based on SID $sid")
        val splitResult = SplitIntoRootedComponents(sh, sid)

        info(s"Split result:\n${splitResult.mkString("\n")}")

        splitResult shouldEqual expectedResult
    }
  }

}
