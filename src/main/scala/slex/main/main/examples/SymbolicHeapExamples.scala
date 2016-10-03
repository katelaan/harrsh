package slex.main.main.examples

import slex.slsyntax._

/**
  * Created by jkatelaa on 10/3/16.
  */
object SymbolicHeapExamples {

  lazy val SingleList = LSeg("x", "y").toSymbolicHeap

  lazy val SplitList = Exists("y", SepCon(LSeg("x", "y"), LSeg("y", "z"))).toSymbolicHeap

  lazy val LassoList = Exists("y", And(SepCon(LSeg("x", "y"), LSeg("y", "y")), PtrNEq("x", "y"))).toSymbolicHeap

}
