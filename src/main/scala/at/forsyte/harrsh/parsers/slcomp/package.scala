package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.Query
import org.antlr.v4.runtime.tree.Trees
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

package object slcomp {

  def parseInput(input: String): Option[SLComp18Parser.StartContext] = {
    //println("\nEvaluating expression " + input)
    val charStream = CharStreams.fromString(input)
    parseStream(charStream)
  }

  def parseFile(filename: String): Option[SLComp18Parser.StartContext] = {
    val charStream = CharStreams.fromFileName(filename)
    parseStream(charStream)
  }

  def parseFileToQuery(filename: String): Option[Query] = {
    val translator = new SidTranslator
    parseFile(filename) map translator.visit map (_.asInstanceOf[Script]) map (_.toQuery(filename))
  }

  def parseStringToQuery(input: String, contentOfFile: Option[String]): Option[Query] = {
    val translator = new SidTranslator
    parseInput(input) map translator.visit map (_.asInstanceOf[Script]) map (_.toQuery(contentOfFile.getOrElse("(no file)")))
  }

  private def parseStream(cs: CharStream) = {
    val lexer = new SLComp18Lexer(cs)
    val tokens = new CommonTokenStream(lexer)
    val parser = new SLComp18Parser(tokens)
    /* Implement listener and use parser */
    val tree = parser.start()
    if (parser.getNumberOfSyntaxErrors == 0) {
      //printTree(tree, parser)
      Some(tree)
    }
    else {
      None
    }
  }

  def printTree(tree: SLComp18Parser.StartContext, parser: SLComp18Parser) = {
    println(Trees.toStringTree(tree, parser))
  }

}
