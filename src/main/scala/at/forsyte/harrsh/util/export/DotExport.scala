package at.forsyte.harrsh.util.export

import java.io.File

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap
import at.forsyte.harrsh.util.IOUtils

import scala.sys.process.stringToProcess

/**
  * Created by jens on 5/21/17.
  */
object DotExport extends HarrshLogging {

  def exportHeap(sh : SymbolicHeap, baseFilename : String) : Unit =
    dotStringToPngFile(baseFilename, SymbolicHeapToDotGraph(sh))

  private def dotStringToPngFile(baseFilename : String, dot : String) : Unit = {
    IOUtils.writeFile(baseFilename + ".dot", dot)
    val command = "dot -Tpng " + baseFilename+".dot"
    logger.trace("Executing " + command)
    (command #> new File(baseFilename+".png")).!

  }

//  def main(args : Array[String]) : Unit = {
//
//    import at.forsyte.harrsh.Implicits._
//    val sh = "x1 -> (y1, y2) * y1 -> (null, null) * y2 -> (null, x2) : {x2 = y3, x1 != x2}".parse
//    exportHeap(sh, "testexport")
//
//  }

}
