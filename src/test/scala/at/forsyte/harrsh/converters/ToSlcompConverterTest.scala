package at.forsyte.harrsh.converters

import at.forsyte.harrsh.{ExampleSIDs, Implicits, TestValues}
import at.forsyte.harrsh.main.EntailmentQuery
import at.forsyte.harrsh.main.ProblemStatus.{Correct, Incorrect}
import at.forsyte.harrsh.parsers.slcomp
import at.forsyte.harrsh.seplog.inductive.{Predicate, SID}
import at.forsyte.harrsh.test.HarrshTableTest

class ToSlcompConverterTest extends HarrshTableTest with Implicits with TestValues {

  val entailmentQueries = Table(
    ("lhs", "rhs", "sid", "status", "file"),
    ("nel(x1,x2) * x2 -> x3 * nel(x3, x4)".parse, "nel(x1, x4)".parse, ExampleSIDs.Nel, Correct, Some("nel.hrs")),
    ("emp : {x1 = x2}".parse, "x1 -> x2 : {x1 != x2}".parse, ExampleSIDs.Nel, Incorrect, Some("nel.hrs")),
    ("tll(x1,x2,x3)".parse, "tll(x1,x2,x3) : {x1 != x2}".parse, ExampleSIDs.TllAcyc, Incorrect, Some("atll.hrs"))
  )

  private def dropRootsAndSort(sid: SID): Seq[Predicate] = {
    sid.preds map (_.copy(rootParam = None)) sortBy(_.head)
  }

  property("Conversion to Slcomp format") {

    forAll(entailmentQueries) {
      (lhs, rhs, sid, status, file) =>

        val query = EntailmentQuery(lhs, rhs, sid, status, file)

        println("Harrsh query representation\n" + query)
        val converted = ToSlcompConverter(query.fileName.get, query)
        val smtString = converted.head._2
        println("Conversion result:\n" + smtString)
        val reparsed = slcomp.parseStringToQuery(smtString, query.fileName)

        reparsed should not be empty

        reparsed.map(_.asInstanceOf[EntailmentQuery]) foreach { result =>
          println("Re-parsed:\n" + result)

          query.status shouldEqual result.status
          query.lhs shouldEqual result.lhs
          query.rhs shouldEqual result.rhs
          dropRootsAndSort(query.sid) shouldEqual dropRootsAndSort(result.sid)
        }


    }

  }

}
