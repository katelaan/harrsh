package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.Implicits._
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 3/5/17.
  */
class ModelCheckingTest extends HarrshTableTest {

  // TODO Add test cases with SIDs that add more pure formulas
  val testCases = Table(
    ("model file", "sid file", "expected result"),
    ("three-elem-list.amd", "list-to-null.sid", true),
    ("ten-elem-list.amd", "list-to-null.sid", true),
    ("three-elem-list.amd", "list-cyc.sid", false),
    ("three-elem-list.amd", "list-unsat.sid", false),
    ("loop.amd", "sll.sid", true),
    ("loop.amd", "sll-acyc.sid", false),
    ("null-terminated-tree.amd", "tree.sid", true),
    ("null-terminated-tree2.amd", "tree.sid", true),
    ("tree.amd", "tree.sid", false),
    ("tree.amd", "tree-with-null-children.sid", true),
    ("tree-with-larger-stack.amd", "tll.sid", false),
    ("tll.amd", "tll.sid", true),
    ("tll2.amd", "tll.sid", true),
    ("tll-wrong.amd", "tll.sid", false),
    //("tll-cyc.amd", "tll.sid", true),
    // A model that introduces a cycle locally (i.e. into its own derivation tree) should not be a locally acyclic TLL
    ("tll-cyc.amd", "tll-acyc.sid", false),
    ("tll-cyc2.amd", "tll-acyc.sid", false),
    // ...but a dangling pointer pointing into an unrelated subtree is fine
    ("tll-cyc3.amd", "tll-acyc.sid", true)
  )

  property("Correctness of the model checker") {
    forAll(testCases) {
      (modelFile : String, sidFile : String, expectedRes : Boolean) =>
        val model = modelFile.parseModel()
        val sid = sidFile.load()

        val modelFormula = ModelToFormula(model)
        Given("Model formula: " + modelFormula + "\n SID: " + sid)

        val res = GreedyUnfoldingModelChecker.isModel(model, sid)
        res shouldEqual expectedRes
    }
  }

//  val (modelFile, sidFile, expectedRes) = ("tll-cyc.amd", "tll.sid", true)
//
//  val model = modelFile.parseModel()
//  val sid = sidFile.load()
//
//  val modelFormula = ModelToFormula(model)
//  println("Model formula: " + modelFormula + "\n SID: " + sid)
//
//  val res = GreedyUnfoldingModelChecker.isModel(model, sid, reportProgress = true)
//  println("Expected res " + expectedRes + ", actual res " + res)
//
}
