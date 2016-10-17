package slex.heapautomata

import slex.seplog.{Emp, PointsTo, PtrEq, SID, SymbolicHeap, call}

/**
  * Created by jens on 10/15/16.
  */
object ExampleSIDs {

  lazy val Sll = SID("sll",
    "Singly-linked list",
    ("sll", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(Emp()))),
    ("sll", SymbolicHeap(Seq(), Seq(PointsTo(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")))
  )

  lazy val EmptyLinearPermuter = SID("a",
    "Non-Allocating Linear FV-Permuter",
    ("a", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("d", fv(2), fv(1))))),
    ("d", SymbolicHeap(Seq(Emp())))
  )

  lazy val NonEmptyLinearPermuter = SID("a",
    "Allocating Linear FV-Permuter",
    ("a", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("d", fv(2), fv(1))))),
    ("d", SymbolicHeap(Seq(PointsTo(fv(1), fv(2)))))
  )

  lazy val NonEmptyBinaryPermuter = SID("a",
    "Binary FV Permuter with Optional Allocation",
    ("a", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("b", fv(1), fv(2)), call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(PtrEq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2)), call("c", fv(2), fv(1))))),
    ("c", SymbolicHeap(Seq(PointsTo(fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(Emp())))
  )
}
