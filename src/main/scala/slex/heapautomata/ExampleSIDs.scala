package slex.heapautomata

import slex.seplog.inductive._

/**
  * Created by jens on 10/15/16.
  */
object ExampleSIDs {

  lazy val Sll = SID("sll",
    "Singly-linked list",
    // sll <= emp : { a = b }
    ("sll", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(emp))),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")))
  )

  lazy val Tree = SID("tree",
    "Null-terminated tree",
    // tree <= x -> (nil, nil)
    ("tree", SymbolicHeap(Seq(ptr(fv(1), nil, nil)))),
    // tree <= ∃ y z . x -> (y, z) * tree(y) * tree(z)
    ("tree", SymbolicHeap(Seq(), Seq(ptr(fv(1), "y", "z"), call("tree", "y"), call("tree", "z")), Seq("y", "z")))
  )

  lazy val Dll = SID("dll",
    "Doubly-linked list",
    // dll <= emp : { a = c, b = d }
    ("dll", SymbolicHeap(Seq(ptreq(fv(1),fv(3)), ptreq(fv(2), fv(4))), Seq(emp))),
    // dll <= ∃ u . a -> (u,b) * dll(u,a,c,d)
    ("dll", SymbolicHeap(Seq(), Seq(ptr(fv(1), "u", fv(2)), call("dll", "u", fv(1), fv(3), fv(4))), Seq("u")))
  )

  lazy val Tll = SID("tll",
    "Tree with linked leaves",
    // tll <= a → (nil nil c) : { a = b }
    ("tll", SymbolicHeap( Seq(ptreq(fv(1), fv(2))), Seq(ptr(fv(1),nil,nil,fv(3))) )),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", SymbolicHeap( Seq(), Seq(ptr(fv(1),"l","r",nil), call("tll", "l", fv(2), "z"), call("tll", "r", "z", fv(3))), Seq("l","r","z") ))
  )

  /**
    * For illustrating the iterative nature; no memory allocation
    */
  lazy val EmptyLinearPermuter = SID("a",
    "Non-Allocating Linear FV-Permuter",
    ("a", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("d", fv(2), fv(1))))),
    ("d", SymbolicHeap(Seq(emp)))
  )

  /**
    * For illustrating the iterative nature; memory allocation
    */
  lazy val NonEmptyLinearPermuter = SID("a",
    "Allocating Linear FV-Permuter",
    ("a", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("d", fv(2), fv(1))))),
    ("d", SymbolicHeap(Seq(ptr(fv(1), fv(2)))))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter = SID("a",
    "Binary FV Permuter with Optional Allocation",
    ("a", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("b", fv(1), fv(2)), call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2)), call("c", fv(2), fv(1))))),
    ("c", SymbolicHeap(Seq(ptr(fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(emp)))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter2 = SID("a",
    "Binary FV Permuter without Allocation",
    ("a", SymbolicHeap(Seq(call("b", fv(1), fv(2)), call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(call("c", fv(1), fv(2)), call("c", fv(2), fv(1))))),
    ("c", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq())),
    ("c", SymbolicHeap(Seq(ptrneq(fv(1), fv(2))), Seq())),
    ("c", SymbolicHeap(Seq(emp)))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter3 = SID("a",
    "Binary FV Permuter with optional allocation and inequalities",
    ("a", SymbolicHeap(Seq(call("b", fv(1), fv(2)), call("b", fv(2), fv(1))))),
    ("b", SymbolicHeap(Seq(call("c", fv(1), fv(2)), call("c", fv(2), fv(1))))),
    ("c", SymbolicHeap(Seq(ptr(fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(emp))),
    ("c", SymbolicHeap(Seq(ptrneq(fv(1), fv(2))), Seq(emp)))
  )

  lazy val UnsatSID = SID("unsat",
    "Unsat singly-linked list",
    ("unsat", SymbolicHeap(Seq(ptrneq(fv(1),fv(2))), Seq(ptr(fv(1), nil), call("sll", fv(1), fv(2))))),
    // sll <= emp : { a = b }
    ("sll", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(emp))),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")))
  )

  lazy val UnsatSID2 = SID("unsat",
    "Tree with unsat leaves",
    ("unsat", SymbolicHeap(Seq(), Seq(ptr(fv(1), "y", "z"), call("tree", "y"), call("tree", "z")), Seq("y", "z"))),
    ("tree", SymbolicHeap(Seq(), Seq(ptr(fv(1), "y", "z"), call("tree", "y"), call("tree", "z")), Seq("y", "z"))),
    ("tree", SymbolicHeap(Seq(ptrneq(nil, nil)), Seq(ptr(fv(1), nil, nil))))
  )

  lazy val OptionallyEstablishedSID = SID("start",
    "Optionally Established SID",
    ("start", SymbolicHeap(Seq(), Seq(call("pred", "z", fv(1)), call("pred", "y", fv(1))), Seq("z","y"))),
    ("pred", SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(emp))),
    ("pred", SymbolicHeap(Seq(ptr(fv(2),fv(1)))))
  )

  lazy val OptionallyEstablishedSID2 = SID("a",
    "Optionally Established SID 2",
    ("a", SymbolicHeap(Seq(), Seq(call("b", "y", "z"), call("b", "y", "w"), call("d", "z", "y")), Seq("y","z","w"))),
    ("b", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2)), call("c", fv(2), fv(1))))),
    ("c", SymbolicHeap(Seq(ptr(fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(emp))),
    ("d", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(emp))),
    ("d", SymbolicHeap(Seq(emp)))
  )

  lazy val OptionallyEstablishedSID3 = SID("a",
    "Optionally Established SID 3",
    ("a", SymbolicHeap(Seq(), Seq(call("b", "y", "z"), call("b", "z", "y")), Seq("y","z"))),
    ("b", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(call("c", fv(1), fv(2)), call("c", fv(2), fv(1))))),
    ("c", SymbolicHeap(Seq(ptr(fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(emp)))
  )

  lazy val OptionallyEstablishedSID4 = SID("a",
    "Optionally Established SID 2",
    ("a", SymbolicHeap(Seq(), Seq(call("b", "w", "z"), call("b", "y", "z"), call("d", "z", fv(1))), Seq("y","z","w"))),
    ("b", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(emp))),
    ("b", SymbolicHeap(Seq(emp))),
    ("d", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(emp))),
    ("d", SymbolicHeap(Seq(emp)))
  )

  lazy val NonEstablishedSID = SID("start",
    "Non-Established SID",
    ("start", SymbolicHeap(Seq(), Seq(call("pred", "z", fv(1))), Seq("z","y"))),
    ("pred", SymbolicHeap(Seq(ptreq(fv(1),fv(2))), Seq(emp))),
    ("pred", SymbolicHeap(Seq(ptr(fv(2),fv(1)))))
  )

  lazy val NonEstablishedSID2 = SID("a",
    "Non-Established SID 2",
    ("a", SymbolicHeap(Seq(), Seq(call("b", "y", "z"), call("b", "y", "w"), call("d", "z", fv(2))), Seq("y","z","w"))),
    ("b", SymbolicHeap(Seq(call("c", fv(1), fv(2))))),
    ("c", SymbolicHeap(Seq(ptr(fv(2), fv(1))))),
    ("c", SymbolicHeap(Seq(emp))),
    ("d", SymbolicHeap(Seq(ptreq(fv(1), fv(2))), Seq(emp))),
    ("d", SymbolicHeap(Seq(emp)))
  )

  lazy val CyclicSll = SID("cll",
    "Cyclic list",
    ("cll", SymbolicHeap(Seq(call("sll", fv(1), fv(1))))),
    ("sll", SymbolicHeap(Seq(ptr(fv(1), fv(2))))),
    ("sll", SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y")))
  )

  lazy val GarbageSll = SID("cll",
    "Garbage list",
    ("sll", SymbolicHeap(Seq(ptr(fv(1), fv(2))))),
    ("sll", SymbolicHeap(Seq(), Seq(ptr(fv(1), "y"), call("sll", "y", fv(2))), Seq("y", "z")))
  )

}
