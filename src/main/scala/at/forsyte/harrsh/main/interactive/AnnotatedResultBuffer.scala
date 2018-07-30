package at.forsyte.harrsh.main.interactive

import at.forsyte.harrsh.util.{StringUtils}

/**
  * Created by jens on 4/3/17.
  */
case class AnnotatedResultBuffer[A](var maxSize : Int, var buffer : Seq[(String,A)] = Seq.empty) {

  def apply(i : Int) : A = buffer(i-1)._2

  private def truncate(newBuffer : Seq[(String,A)]) : Unit = {
    buffer = newBuffer.take(maxSize)
  }

  def setSize(size : Int) : Unit = {
    maxSize = size
    truncate(buffer)
  }

  def add(desc : String, elem : A) : Unit = {
    truncate((desc,elem) +: buffer)
  }

  def addAll(desc : String, elems : Iterable[A]) : Unit = {
    truncate((elems map (e => (desc,e))).toSeq ++ buffer)
  }

  def clear : Unit = {
    buffer = Seq.empty
  }

  def summarize : String = {
    val headings = Seq("Index","Description","Element")
    val entries : Seq[Seq[String]] = for {
      ((s,a),i) <- buffer.zipWithIndex
    } yield Seq((i+1).toString, s, a.toString.replace('\n', ' ').take(120))
    val cols = Seq(7, Math.max(11,buffer.map(_._1).map(_.size).max + 2), Math.min(120, buffer.map(_._2).map(_.toString.size).max + 2))

    StringUtils.toTable(headings,cols,entries)
  }

}
