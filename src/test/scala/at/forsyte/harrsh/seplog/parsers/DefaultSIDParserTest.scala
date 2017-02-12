package at.forsyte.harrsh.seplog.parsers

import at.forsyte.harrsh.test.HarrshTableTest

/**
  * Created by jkatelaa on 10/20/16.
  */
class DefaultSIDParserTest extends HarrshTableTest {

  import DefaultSIDParser._

  val Success = true
  val Failure = false

  val inputs = Table(
    ("parser", "input", "result"),
    (parseRule, "sll <= emp : {x1 = x2}", Success),
    (parseRule, "sll <= x1 -> y * sll(y,x2)", Success),
    (parseRule, "dll <= emp : { x1 = x3, x2 = x4 }", Success),
    (parseRule, "dll <= x1 -> (u,x2) * dll(u,x1,x3,x4)", Success),
    (parseSID, fullExample, Success),
    (parseSID, fullExample2, Success)
  )

  property("The Cyclist SID parser should work") {
    forAll(inputs) {
      (parser, input, expectedResult) =>
        val parseResult = parseAll(parser, input)

        info("" + parseResult)

        if (expectedResult) {
          parseResult.successful should be(expectedResult)
        }
    }
  }


  private def fullExample =
    """
      |sll <= emp : {x1 = x2} ;
      |sll <= x1 -> y * sll(y,x2)
    """.stripMargin

  private def fullExample2 =
    """
      |tree <= x1 -> (nil, nil) ;
      |tree <= x1 -> (y, z) * tree(y) * tree(z)
    """.stripMargin

}
