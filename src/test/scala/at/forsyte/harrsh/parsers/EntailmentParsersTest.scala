package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.test.HarrshTest

class EntailmentParsersTest extends HarrshTest {

  behavior of "entailment parser"

  val parser = EntailmentParsers.DefaultEntailmentParser

  val sllEntailsSll = """query {
                        |  sll(x1, x2) |= sll(x1, x2)
                        |}
                        |sid {
                        |  sll <= x1 -> x2 ;
                        |  sll <= x1 -> y * sll(y,x2)
                        |}
                        |info {
                        |  status = true
                        |}
                        |""".stripMargin('|')

  val twoPtrsEntailsSll = """query {
                            |  twoptrs(x1, x2) |= sll(x1, x2)
                            |}
                            |sid {
                            |  sll <= x1 -> x2 ;
                            |  sll <= x1 -> y * sll(y,x2) ;
                            |  oneptr <= x1 -> x2 ;
                            |  twoptrs <= x1 -> y * oneptr(y, x2)
                            |}
                            |info {
                            |  status = true
                            |}""".stripMargin('|')

  it should "parse valid inputs" in {

    for {
      input <- Seq(sllEntailsSll, twoPtrsEntailsSll)
    } {
      val res = parser.run(input)
      info(s"Parse result: $res")
      res should not be empty
    }

  }

  it should "convert valid inputs to entailment instances" in {

    for {
      input <- Seq(sllEntailsSll, twoPtrsEntailsSll)
    } {
      val res = EntailmentParsers.harrshEntailmentFormatToProcessedInstance(input, computeSeparateSidsForEachSide = true)
      info(s"Entailment instance: $res")
      res should not be empty
    }

  }

}
