package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.main.Var._
import at.forsyte.harrsh.main._

/**
  * Created by jens on 10/15/16.
  */
object ExampleSIDs {

  lazy val Sll = SID("sll",
    "Singly-linked list",
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(emp))),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1)), call("sll", qv(1), mkVar(2)))))
  )

  lazy val Tree = SID("tree",
    "Null-terminated tree",
    // tree <= x -> (nil, nil)
    ("tree", Seq.empty,  SymbolicHeap(Seq(ptr(mkVar(1), nil, nil)))),
    // tree <= ∃ y z . x -> (y, z) * tree(y) * tree(z)
    ("tree", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1), qv(2)), call("tree", qv(1)), call("tree", qv(2)))))
  )

  lazy val Dll = SID("dll",
    "Doubly-linked list",
    // dll <= emp : { a = c, b = d }
    ("dll", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(3)), ptreq(mkVar(2), mkVar(4))), Seq(emp))),
    // dll <= ∃ u . a -> (u,b) * dll(u,a,c,d)
    ("dll", Seq("u"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1), mkVar(2)), call("dll", qv(1), mkVar(1), mkVar(3), mkVar(4)))))
  )

  lazy val Tll = SID("tll",
    "Tree with linked leaves",
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(ptr(mkVar(1),nil,nil,mkVar(3))))),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap( Seq(), Seq(ptr(mkVar(1),qv(1),qv(2),nil), call("tll", qv(1), mkVar(2), qv(3)), call("tll", qv(2), qv(3), mkVar(3))) ))
  )

  /**
    * For illustrating the iterative nature; no memory allocation
    */
  lazy val EmptyLinearPermuter = SID("a",
    "Non-Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("b", mkVar(2), mkVar(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("c", mkVar(1), mkVar(2))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("d", mkVar(2), mkVar(1))))),
    ("d", Seq.empty, SymbolicHeap(Seq(emp)))
  )

  /**
    * For illustrating the iterative nature; memory allocation
    */
  lazy val NonEmptyLinearPermuter = SID("a",
    "Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("b", mkVar(2), mkVar(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("c", mkVar(1), mkVar(2))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("d", mkVar(2), mkVar(1))))),
    ("d", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2)))))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter = SID("a",
    "Binary FV Permuter with Optional Allocation",
    ("a", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("b", mkVar(1), mkVar(2)), call("b", mkVar(2), mkVar(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("c", mkVar(1), mkVar(2)), call("c", mkVar(2), mkVar(1))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2))))),
    ("c", Seq.empty, SymbolicHeap(Seq(emp)))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter2 = SID("a",
    "Binary FV Permuter without Allocation",
    ("a", Seq.empty, SymbolicHeap(Seq(call("b", mkVar(1), mkVar(2)), call("b", mkVar(2), mkVar(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(call("c", mkVar(1), mkVar(2)), call("c", mkVar(2), mkVar(1))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq())),
    ("c", Seq.empty, SymbolicHeap(Seq(ptrneq(mkVar(1), mkVar(2))), Seq())),
    ("c", Seq.empty, SymbolicHeap(Seq(emp)))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter3 = SID("a",
    "Binary FV Permuter with optional allocation and inequalities",
    ("a", Seq.empty, SymbolicHeap(Seq(call("b", mkVar(1), mkVar(2)), call("b", mkVar(2), mkVar(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(call("c", mkVar(1), mkVar(2)), call("c", mkVar(2), mkVar(1))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2))))),
    ("c", Seq.empty, SymbolicHeap(Seq(emp))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptrneq(mkVar(1), mkVar(2))), Seq(emp)))
  )

  lazy val UnsatSID = SID("unsat",
    "Unsat singly-linked list",
    ("unsat", Seq.empty, SymbolicHeap(Seq(ptrneq(mkVar(1),mkVar(2))), Seq(ptr(mkVar(1), nil), call("sll", mkVar(1), mkVar(2))))),
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(emp))),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1)), call("sll", qv(1), mkVar(2)))))
  )

  lazy val UnsatSID2 = SID("unsat",
    "Tree with unsat leaves",
    ("unsat", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1), qv(2)), call("tree", qv(1)), call("tree", qv(2))))),
    ("tree", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1), qv(2)), call("tree", qv(1)), call("tree", qv(2))))),
    ("tree", Seq.empty, SymbolicHeap(Seq(ptrneq(nil, nil)), Seq(ptr(mkVar(1), nil, nil))))
  )

  lazy val OptionallyEstablishedSID = SID("start",
    "Optionally Established SID",
    ("start", Seq("z", "y"), SymbolicHeap(Seq(), Seq(call("pred", qv(1), mkVar(1)), call("pred", qv(2), mkVar(1))))),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2))), Seq(emp))),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(2),mkVar(1)))))
  )

  lazy val OptionallyEstablishedSID2 = SID("a",
    "Optionally Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(Seq(), Seq(call("b", qv(1), qv(2)), call("b", qv(1), qv(3)), call("d", qv(2), qv(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("c", mkVar(1), mkVar(2)), call("c", mkVar(2), mkVar(1))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2))))),
    ("c", Seq.empty, SymbolicHeap(Seq(emp))),
    ("d", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(emp))),
    ("d", Seq.empty, SymbolicHeap(Seq(emp)))
  )

  lazy val OptionallyEstablishedSID3 = SID("a",
    "Optionally Established SID 3",
    ("a", Seq("y","z"), SymbolicHeap(Seq(), Seq(call("b", qv(1), qv(2)), call("b", qv(2), qv(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(call("c", mkVar(1), mkVar(2)), call("c", mkVar(2), mkVar(1))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2))))),
    ("c", Seq.empty, SymbolicHeap(Seq(emp)))
  )

  lazy val OptionallyEstablishedSID4 = SID("a",
    "Optionally Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(Seq(), Seq(call("b", qv(3), qv(2)), call("b", qv(1), qv(2)), call("d", qv(2), mkVar(1))))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(emp))),
    ("b", Seq.empty, SymbolicHeap(Seq(emp))),
    ("d", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(emp))),
    ("d", Seq.empty, SymbolicHeap(Seq(emp)))
  )

  lazy val NonEstablishedSID = SID("start",
    "Non-Established SID",
    ("start", Seq("z","y"), SymbolicHeap(Seq(), Seq(call("pred", qv(1), mkVar(1))), 1, Seq(-1,-2))),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1),mkVar(2))), Seq(emp))),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(2),mkVar(1)))))
  )

  lazy val NonEstablishedSID2 = SID("a",
    "Non-Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(Seq(), Seq(call("b", qv(1), qv(2)), call("b", qv(1), qv(3)), call("d", qv(2), mkVar(2))))),
    ("b", Seq.empty, SymbolicHeap(Seq(call("c", mkVar(1), mkVar(2))))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(2), mkVar(1))))),
    ("c", Seq.empty, SymbolicHeap(Seq(emp))),
    ("d", Seq.empty, SymbolicHeap(Seq(ptreq(mkVar(1), mkVar(2))), Seq(emp))),
    ("d", Seq.empty, SymbolicHeap(Seq(emp)))
  )

  lazy val CyclicSll = SID("cll",
    "Cyclic list",
    ("cll", Seq.empty, SymbolicHeap(Seq(call("sll", mkVar(1), mkVar(1))))),
    ("sll", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2))))),
    ("sll", Seq("y"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1)), call("sll", qv(1), mkVar(2)))))
  )

  lazy val GarbageSll = SID("cll",
    "Garbage list",
    ("sll", Seq.empty, SymbolicHeap(Seq(ptr(mkVar(1), mkVar(2))))),
    ("sll", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(mkVar(1), qv(1)), call("sll", qv(1), mkVar(2)))))
  )

}
