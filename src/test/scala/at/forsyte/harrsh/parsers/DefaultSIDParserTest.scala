package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.test.HarrshTableTest
import at.forsyte.harrsh.parsers.SIDParsers.DefaultSIDParser

/**
  * Created by jkatelaa on 10/20/16.
  */
class DefaultSIDParserTest extends HarrshTableTest {

  val Success = true
  val Failure = false

  val inputs = Table(
    ("input", "result"),
    ("sll <= emp : {x1 = x2}", Success),
    ("sll <= x1 -> y * sll(y,x2)", Success),
    ("dll <= emp : { x1 = x3, x2 = x4 }", Success),
    ("dll <= x1 -> (u,x2) * dll(u,x1,x3,x4)", Success),
    (fullExample, Success),
    (fullExample2, Success)
  )

  property("The Cyclist SID parser should work") {
    forAll(inputs) {
      (input, expectedResult) =>
        val parseResult = DefaultSIDParser.runOnSID(input)

        info("" + parseResult)

        if (expectedResult) {
          parseResult.isDefined should be(expectedResult)
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
