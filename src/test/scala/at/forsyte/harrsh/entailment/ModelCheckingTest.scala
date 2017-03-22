package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.MainIO
import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 3/5/17.
  */
class ModelCheckingTest extends HarrshTableTest {

  // TODO Add test cases with SIDs that add more pure formulas
  val testCases = Table(
    ("model file", "sid file", "expected result"),
    ("examples/models/three-elem-list.amd", "examples/symbolicheaps/list-to-null.sid", true),
    ("examples/models/ten-elem-list.amd", "examples/symbolicheaps/list-to-null.sid", true),
    ("examples/models/three-elem-list.amd", "examples/symbolicheaps/list-cyc.sid", false),
    ("examples/models/three-elem-list.amd", "examples/symbolicheaps/list-unsat.sid", false),
    ("examples/models/null-terminated-tree.amd", "examples/datastructures/tree.sid", true),
    ("examples/models/null-terminated-tree2.amd", "examples/datastructures/tree.sid", true),
    ("examples/models/tree.amd", "examples/datastructures/tree.sid", false),
    ("examples/models/tree.amd", "examples/datastructures/tree-with-null-children.sid", true),
    ("examples/models/tree-with-larger-stack.amd", "examples/datastructures/tll.sid", false),
    ("examples/models/tll.amd", "examples/datastructures/tll.sid", true),
    ("examples/models/tll2.amd", "examples/datastructures/tll.sid", true),
    ("examples/models/tll-wrong.amd", "examples/datastructures/tll.sid", false),
    ("examples/models/tll-cyc.amd", "examples/datastructures/tll.sid", true),
    ("examples/models/tll-cyc.amd", "examples/datastructures/tll-acyc.sid", false)
  )

//  property("Correctness of the model checker") {
//    forAll(testCases) {
//      (modelFile : String, sidFile : String, expectedRes : Boolean) =>
//        val model = MainIO.getModelFromFile(modelFile)
//        val (sid,_) = MainIO.getSidFromFile(sidFile)
//
//        val modelFormula = ModelToFormula(model)
//        Given("Model formula: " + modelFormula + "\n SID: " + sid)
//
//        val res = GreedyUnfoldingModelChecker.isModel(model, sid)
//        res shouldEqual expectedRes
//    }
//  }

  val (modelFile, sidFile, expectedRes) = ("examples/models/tll2.amd", "examples/datastructures/tll.sid", true)

  val model = MainIO.getModelFromFile(modelFile)
  val (sid,_) = MainIO.getSidFromFile(sidFile)

  val modelFormula = ModelToFormula(model)
  println("Model formula: " + modelFormula + "\n SID: " + sid)

  val res = GreedyUnfoldingModelChecker.isModel(model, sid)
  println("Expected res " + expectedRes + ", actual res " + res)
  
}
