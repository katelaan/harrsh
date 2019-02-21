package at.forsyte.harrsh.util

import at.forsyte.harrsh.seplog.Var.Naming

trait ToLatex[-A] {
  def toLatex(a: A, naming: Naming): String
}

object ToLatex {
  def apply[A](implicit tl: ToLatex[A]): ToLatex[A] = tl

  def toLatex[A: ToLatex](a: A, naming: Naming = Naming.DefaultNaming): String = ToLatex[A].toLatex(a, naming)

  implicit class ToLatexOps[A: ToLatex](a: A) {
    def toLatex: String = ToLatex[A].toLatex(a, Naming.DefaultNaming)
    def toLatex(naming: Naming): String = ToLatex[A].toLatex(a, naming)
  }
}
