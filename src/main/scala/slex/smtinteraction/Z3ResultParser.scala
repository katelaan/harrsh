package slex.smtinteraction

import slex.models.{MapStack, Stack}

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by jkatelaa on 10/4/16.
  */
object Z3ResultParser extends JavaTokenParsers {

  def run(input : String) : Option[SmtOutput] = parseAll(parseZ3Result, input) match {
    case Success(result, next) => Some(result)
    case Failure(msg,_) => println("FAILURE: " + msg); None
    case Error(msg,_) => println("ERROR: " + msg); None
  }

  def parseZ3Result : Parser[SmtOutput] = statusParser ~ opt(modelParser) ^^ {
    case a ~ b => (a,b)
  }

  def statusParser : Parser[SatStatus] = "sat" ^^ (_ => Sat()) | "unsat" ^^ (_ => UnSat())

  def modelParser : Parser[Stack] = inParens("model" ~> (rep(funDefParser) ^^ {
    res => MapStack(Map() ++ res)
  }))

  def funDefParser : Parser[(String, Int)] = ("(" ~> "define-fun" ~> ident) ~ ("(" ~> ")" ~> ("Int" ~> smtNumberParser) <~ ")") ^^ {
    case a ~ b => (a,b)
  }

  def smtNumberParser : Parser[Int] = wholeNumberParser | inParens("-" ~> wholeNumberParser ^^ { i => -i })

  def wholeNumberParser : Parser[Int] = wholeNumber ^^ { Integer.valueOf(_) }

  def inParens[A](p : Parser[A]) : Parser[A] = "(" ~> (p <~ ")")

  // TODO Move the tests somewhere they actually belong...
  def testSmtParser() : Unit = {
    println(Z3ResultParser.run(input0))
    println(Z3ResultParser.run(input0a))
    println(Z3ResultParser.parseAll(Z3ResultParser.funDefParser, fundef))
    println(Z3ResultParser.run(input1))
    println(Z3ResultParser.run(input2))
  }

  lazy val fundef = """(define-fun iX () Int
                       0)"""

  lazy val input0 = "unsat"

  lazy val input0a = "sat"

  lazy val input1 = """sat
                     (model
                       (define-fun iX () Int
                         0)
                     )"""

  lazy val input2 = """sat
                     (model
                       (define-fun iX () Int
                         0)
                       (define-fun p () Int
                         (- 1))
                       (define-fun q () Int
                         0)
                       (define-fun qX () Int
                         (- 1))
                       (define-fun null () Int
                         0)
                       (define-fun n () Int
                         1)
                       (define-fun i () Int
                         1)
                     )"""

}
