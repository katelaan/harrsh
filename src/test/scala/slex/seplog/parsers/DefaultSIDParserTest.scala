package slex.seplog.parsers

import slex.test.SlexTableTest

/**
  * Created by jkatelaa on 10/20/16.
  */
class DefaultSIDParserTest extends SlexTableTest {

  import DefaultSIDParser._

  val Success = true
  val Failure = false

  val inputs = Table(
    ("parser", "input", "result"),
    (parseRule, "sll <= emp : {x1 = x2}", Success),
    (parseRule, "sll <= x1 -> y * sll(y,x2)", Success),
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
