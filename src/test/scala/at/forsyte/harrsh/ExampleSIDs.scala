package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
import at.forsyte.harrsh.seplog.Var.nil

/**
  * Created by jens on 10/15/16.
  */
object ExampleSIDs extends TestValues with AtomConstructorFunctions {

  lazy val Sll = SID("sll",
    "Singly-linked list",
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq.empty)),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(Seq(), Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))))
  )

  lazy val Tree = SID("tree",
    "Null-terminated tree",
    // tree <= x -> (nil, nil)
    ("tree", Seq.empty,  SymbolicHeap(Seq(ptr(x1, nil, nil)))),
    // tree <= ∃ y z . x -> (y, z) * tree(y) * tree(z)
    ("tree", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(x1, y1, y2)), Seq(call("tree", y1), call("tree", y2))))
  )

  lazy val Dll = SID("dll",
    "Doubly-linked list",
    // dll <= emp : { a = c, b = d }
    ("dll", Seq.empty, SymbolicHeap(Seq(ptreq(x1,x3), ptreq(x2, x4)), Seq.empty, Seq.empty)),
    // dll <= ∃ u . a -> (u,b) * dll(u,a,c,d)
    ("dll", Seq("u"), SymbolicHeap(Seq(), Seq(ptr(x1, y1, x2)), Seq(call("dll", y1, x1, x3, x4))))
  )

  lazy val Tll = SID("tll",
    "Tree with linked leaves",
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq(ptr(x1,nil,nil,x3)), Seq.empty)),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap( Seq(), Seq(ptr(x1,y1,y2,nil)), Seq(call("tll", y1, x2, y3), call("tll", y2, y3, x3)) ))
  )

  lazy val TllAcycExtraNeq = SID("tll",
    "Acyclic tree with linked leaves, overspecified",
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2), ptrneq(x1, x3)), Seq(ptr(x1,nil,nil,x3)), Seq.empty)),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap( Seq(ptrneq(x1, x3), ptrneq(x2, x3)), Seq(ptr(x1,y1,y2,nil)), Seq(call("tll", y1, x2, y3), call("tll", y2, y3, x3)) ))
  )

  lazy val TllAcyc = SID("tll",
    "Acyclic tree with linked leaves",
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2), ptrneq(x1, x3)), Seq(ptr(x1,nil,nil,x3)), Seq.empty)),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap( Seq(ptrneq(x1, x3)), Seq(ptr(x1,y1,y2,nil)), Seq(call("tll", y1, x2, y3), call("tll", y2, y3, x3)) ))
  )

  /**
    * For illustrating the iterative nature; no memory allocation
    */
  lazy val EmptyLinearPermuter = SID("a",
    "Non-Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("b", x2, x1)))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("c", x1, x2)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("d", x2, x1)))),
    ("d", Seq.empty, SymbolicHeap(Seq.empty))
  )

  /**
    * For illustrating the iterative nature; memory allocation
    */
  lazy val NonEmptyLinearPermuter = SID("a",
    "Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("b", x2, x1)))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("c", x1, x2)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("d", x2, x1)))),
    ("d", Seq.empty, SymbolicHeap(Seq(ptr(x1, x2))))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter = SID("a",
    "Binary FV Permuter with Optional Allocation",
    ("a", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("b", x1, x2), call("b", x2, x1)))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("c", x1, x2), call("c", x2, x1)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(x1, x2)))),
    ("c", Seq.empty, SymbolicHeap(Seq.empty))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter2 = SID("a",
    "Binary FV Permuter without Allocation",
    ("a", Seq.empty, SymbolicHeap(Seq.empty, Seq(call("b", x1, x2), call("b", x2, x1)))),
    ("b", Seq.empty, SymbolicHeap(Seq.empty, Seq(call("c", x1, x2), call("c", x2, x1)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq.empty)),
    ("c", Seq.empty, SymbolicHeap(Seq(ptrneq(x1, x2)), Seq.empty, Seq.empty)),
    ("c", Seq.empty, SymbolicHeap(Seq.empty))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter3 = SID("a",
    "Binary FV Permuter with optional allocation and inequalities",
    ("a", Seq.empty, SymbolicHeap(Seq.empty, Seq(call("b", x1, x2), call("b", x2, x1)))),
    ("b", Seq.empty, SymbolicHeap(Seq.empty, Seq(call("c", x1, x2), call("c", x2, x1)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(x1, x2)))),
    ("c", Seq.empty, SymbolicHeap(Seq.empty)),
    ("c", Seq.empty, SymbolicHeap(Seq(ptrneq(x1, x2)), Seq.empty, Seq.empty))
  )

  lazy val UnsatSID = SID("unsat",
    "Unsat singly-linked list",
    ("unsat", Seq.empty, SymbolicHeap(Seq(ptrneq(x1,x2)), Seq(ptr(x1, nil)), Seq(call("sll", x1, x2)))),
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq.empty)),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(Seq(), Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))))
  )

  lazy val UnsatSID2 = SID("unsat",
    "Tree with unsat leaves",
    ("unsat", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(x1, y1, y2)), Seq(call("tree", y1), call("tree", y2)))),
    ("tree", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(x1, y1, y2)), Seq(call("tree", y1), call("tree", y2)))),
    ("tree", Seq.empty, SymbolicHeap(Seq(ptrneq(nil, nil)), Seq(ptr(x1, nil, nil)), Seq.empty))
  )

  lazy val OptionallyEstablishedSID = SID("start",
    "Optionally Established SID",
    ("start", Seq("z", "y"), SymbolicHeap(Seq(), Seq(call("pred", y1, x1), call("pred", y2, x1)))),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptreq(x1,x2)), Seq.empty, Seq.empty)),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptr(x2,x1))))
  )

  lazy val OptionallyEstablishedSID2 = SID("a",
    "Optionally Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(Seq(), Seq(call("b", y1, y2), call("b", y1, y3), call("d", y2, y1)))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("c", x1, x2), call("c", x2, x1)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(x1, x2)))),
    ("c", Seq.empty, SymbolicHeap(Seq.empty)),
    ("d", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq.empty)),
    ("d", Seq.empty, SymbolicHeap(Seq.empty))
  )

  lazy val OptionallyEstablishedSID3 = SID("a",
    "Optionally Established SID 3",
    ("a", Seq("y","z"), SymbolicHeap(Seq(), Seq(call("b", y1, y2), call("b", y2, y1)))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq(call("c", x1, x2), call("c", x2, x1)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(x1, x2)))),
    ("c", Seq.empty, SymbolicHeap(Seq.empty))
  )

  lazy val OptionallyEstablishedSID4 = SID("a",
    "Optionally Established SID 4",
    ("a", Seq("y","z","w"), SymbolicHeap(Seq(), Seq(call("b", y3, y2), call("b", y1, y2), call("d", y2, x1)))),
    ("b", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq.empty)),
    ("b", Seq.empty, SymbolicHeap(Seq.empty)),
    ("d", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq.empty)),
    ("d", Seq.empty, SymbolicHeap(Seq.empty))
  )

  lazy val NonEstablishedSID = SID("start",
    "Non-Established SID",
    ("start", Seq("z","y"), SymbolicHeap.fromFullDescription(Seq.empty, Seq.empty, Seq(call("pred", y1, x1)), 1, Seq(y1,y2))),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptreq(x1,x2)), Seq.empty, Seq.empty)),
    ("pred", Seq.empty, SymbolicHeap(Seq(ptr(x2,x1))))
  )

  lazy val NonEstablishedSID2 = SID("a",
    "Non-Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(Seq(), Seq(call("b", y1, y2), call("b", y1, y3), call("d", y2, x2)))),
    ("b", Seq.empty, SymbolicHeap(Seq.empty, Seq(call("c", x1, x2)))),
    ("c", Seq.empty, SymbolicHeap(Seq(ptr(x2, x1)))),
    ("c", Seq.empty, SymbolicHeap(Seq.empty)),
    ("d", Seq.empty, SymbolicHeap(Seq(ptreq(x1, x2)), Seq.empty, Seq.empty)),
    ("d", Seq.empty, SymbolicHeap(Seq.empty))
  )

  lazy val CyclicSll = SID("cll",
    "Cyclic list",
    ("cll", Seq.empty, SymbolicHeap(Seq.empty, Seq(call("sll", x1, x1)))),
    ("sll", Seq.empty, SymbolicHeap(Seq(ptr(x1, x2)))),
    ("sll", Seq("y"), SymbolicHeap(Seq(), Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))))
  )

  lazy val GarbageSll = SID("cll",
    "Garbage list",
    ("sll", Seq.empty, SymbolicHeap(Seq(ptr(x1, x2)))),
    ("sll", Seq("y", "z"), SymbolicHeap(Seq(), Seq(ptr(x1, y1)), Seq(call("sll", y1, x2))))
  )

}
