package at.forsyte.harrsh.util.export

import scala.xml.Elem
import at.forsyte.harrsh.entailment.learning.{ObservationTable, TableEntry}
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}
import at.forsyte.harrsh.util.IOUtils

/**
  * Created by jens on 5/22/17.
  */
object ObservationTableToHtml {

  def apply(obs : ObservationTable, dirname : String) : Unit = {
    IOUtils.mkDir(dirname)
    val html = HtmlGenerator(dirname).run(obs)
    IOUtils.writeFile(dirname + "/index.html", html.toString)
  }

  case class HtmlGenerator(dirname : String) {

    def run(obs : ObservationTable) : Elem = <html>
      <head>
        <meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
        <style>{css}</style>
      </head>
      <body>
        <div><code>{obs.sid}</code></div>
        <table>
          <tr><th>State ID</th><th>Representative(s)</th><th>Minimal Extension(s)</th><th>Final?</th><th>Meta</th></tr>
          {
          obs.entries.sortBy(_.discoveredInIteration).zipWithIndex map (pair => entryToHtml(pair._2 + 1, pair._1))
          }
        </table>
      </body>
    </html>

    private def entryToHtml(id : Int, entry : TableEntry) : Elem = <tr>
      <td>{id}</td>
      <td>{repsToPng(id, entry.reps)}</td>
      <td>{extsToPng(id, entry.exts)}</td>
      <td>{if (entry.isFinal) "Yes" else "No"}</td>
      <td>{"free vars = " + entry.numFV + ","}<br/>{"iteration = " + entry.discoveredInIteration}<br/>{"introduced in postprocessing = " + entry.introducedThroughClosure}</td>
      </tr>

    private def repsToPng(id : Int, reps : Set[SymbolicHeap]) : Elem = {
      def repFileName(ix : Int) = "rep" + id + "_" + ix

      <span>
        {
          reps.zipWithIndex map {
            case (sh, ix) => heapTable(sh, repFileName(ix))
          }
        }
      </span>
    }

    private def extsToPng(id : Int, exts : Set[(SymbolicHeap,PredCall)]) : Elem = {
      def extFileName(ix : Int) = "ext" + id + "_" + ix

      <span>
        {
          exts.zipWithIndex map {
            case ((sh,call), ix) =>
              heapTable(sh.copy(predCalls = Seq(call)), extFileName(ix))
          }
        }
      </span>
    }

    private def heapTable(sh : SymbolicHeap, baseFile : String) : Elem = {
      DotExport.exportHeap(sh, dirname + "/" + baseFile)
      <table><tr><td>{sh.toString}</td></tr><tr><td><img src={baseFile + ".png"}/></td></tr></table>
    }

    private val css =
      """
        |code {
        |  display: block;
        |  white-space: pre-wrap;
        |  padding-left: 100px;
        |  margin-bottom: 20px;
        |  font-size: 120%;
        |  background: #dddddd;
        |}
        |table {
        |  border-collapse: collapse;
        |}
        |table,th,td {
        |  border: 1px dotted black;
        |  padding: 10px;
        |}
        |img {
        |  padding: 5px;
        |}
      """.stripMargin

  }

}
