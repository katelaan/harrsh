package at.forsyte.harrsh.converters

import at.forsyte.harrsh.{ExampleSIDs, Implicits, TestValues}
import at.forsyte.harrsh.main.EntailmentQuery
import at.forsyte.harrsh.main.ProblemStatus.{Correct, Incorrect}
import at.forsyte.harrsh.parsers.{QueryParser, slcomp}
import at.forsyte.harrsh.seplog.inductive.{Predicate, SID, SidLike}
import at.forsyte.harrsh.test.HarrshTableTest

class ToSlcompConverterTest extends HarrshTableTest with Implicits with TestValues {

  val entailmentQueries = Table(
    ("lhs", "rhs", "sid", "status", "file"),
    ("nel(x1,x2) * x2 -> x3 * nel(x3, x4)".parse, "nel(x1, x4)".parse, ExampleSIDs.Nel.underlying, Correct, Some("nel.hrs")),
    ("emp : {x1 = x2}".parse, "x1 -> x2 : {x1 != x2}".parse, ExampleSIDs.Nel.underlying, Incorrect, Some("nel.hrs")),
    ("tll(x1,x2,x3)".parse, "tll(x1,x2,x3) : {x1 != x2}".parse, ExampleSIDs.TllAcyc.underlying, Incorrect, Some("atll.hrs")),
    ("emp".parse, "dll(x1,x2,x3,x4)".parse, ExampleSIDs.NeAcycDll.underlying, Incorrect, Some("dll.hrs")),
    ("odd(x1,x2) * odd(x2, x3)".parse, "even(x1,x3) : {x1 != x3}".parse, ExampleSIDs.OddList.underlying, Correct, Some("odd.hrs")),
    ("emp".parse, "emp".parse, ExampleSIDs.OptionallyEstablishedSID2, Correct, Some("strange.hrs"))
  )

  private def sort(sid: SidLike): Seq[Predicate] = {
    sid.preds sortBy (_.head)
  }

  private def checkIdentityForQuery(query: EntailmentQuery): Unit = {
    Given("Harrsh query representation\n" + query)
    val converted = ToSlcompConverter(query.fileName.get, query)
    val smtString = converted.head._2
    info("Conversion result:\n" + smtString)
    val reparsed = slcomp.parseStringToQuery(smtString, query.fileName)

    reparsed should not be empty

    reparsed.map(_.asInstanceOf[EntailmentQuery]) foreach { result =>
      info("Re-parsed:\n" + result)

      query.status shouldEqual result.status
      query.lhs shouldEqual result.lhs
      query.rhs shouldEqual result.rhs
      sort(query.sid) shouldEqual sort(result.sid)
    }
  }

  property("Conversion of example SIDs to Slcomp format") {

    forAll(entailmentQueries) {
      (lhs, rhs, sid, status, file) =>

        val query = EntailmentQuery(lhs, rhs, sid, status, file)
        checkIdentityForQuery(query)

    }

  }

  val harrshBenchmarks = Table(
    "file",
    "examples/entailment/trees_singly_linked/treefragment-plus-tree_tree.hrs",
    "examples/entailment/trees_doubly_linked/greater-ptree_leaf-tree.hrs",
    "examples/entailment/trees_doubly_linked/leaf-tree_greater-ptree.hrs",
    "examples/entailment/trees_doubly_linked/leaf-tree_ptree.hrs",
    "examples/entailment/trees_doubly_linked/ptree_leaf-tree.hrs",
    "examples/entailment/trees_doubly_linked/small-ptree_leaf-tree.hrs",
    "examples/entailment/trees_with_linked_leaves/acyc-tll_tll.hrs",
    "examples/entailment/trees_with_linked_leaves/tll_acyc-tll.hrs",
    "examples/entailment/trees_with_linked_leaves/tll-parent_tll-parent.hrs",
    "examples/entailment/k-grids/2-dl-grid.hrs",
    "examples/entailment/k-grids/dlgrid-left-right.hrs"
  )

  property("Conversion of Harrsh benchmarks to Slcomp format") {

    forAll(harrshBenchmarks) {
      file =>
        val query = QueryParser(file).asInstanceOf[EntailmentQuery]
        checkIdentityForQuery(query)
    }

  }

}
