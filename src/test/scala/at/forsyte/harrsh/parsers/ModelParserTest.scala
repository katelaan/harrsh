package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jens on 2/25/17.
  */
class ModelParserTest extends HarrshTableTest {

  import ModelParser._

  val Success = true
  val Failure = false

  val inputs = Table(
    ("parser", "input", "result"),
    (parseStack, stack1, Success),
    (parseHeap, heap1, Success),
    (parseModel, model1, Success)
  )

  property ("The model parser should work") {
    forAll(inputs) {
      (parser, input, expectedResult) =>
        val parseResult = parseAll(parser, input)

        info(""+parseResult)

        if (expectedResult) {
          parseResult.successful should be (expectedResult)
        }
    }
  }

  lazy val stack1 = """STACK {
    x1 -> 5;
    x2 -> 42;
    x3 -> 55;
  }"""

  lazy val heap1 = """HEAP {
    5 -> 42, 55;
    42 -> 55;
    55 -> 3;
    3 -> 0;
  }"""

  lazy val model1 = stack1 + "\n" + heap1


}
