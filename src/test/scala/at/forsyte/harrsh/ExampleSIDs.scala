package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jens on 10/15/16.
  */
object ExampleSIDs extends TestValues {

  lazy val Sll = SID("sll",
    "Singly-linked list",
    Map("sll" -> x1),
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(x1 =:= x2)),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

  lazy val Nel = SID("nel",
    "Nonempty list",
    Map("nel" -> x1),
    // nel <= emp : { a = b }
    ("nel", Seq.empty, SymbolicHeap(x1 -> x2)),
    // nel <= ∃ y . a -> y * nel(y, b)
    ("nel", Seq("y"), SymbolicHeap(x1 -> y1, P("nel")(y1, x2)))
  )

  lazy val Tree = SID("tree",
    "Null-terminated tree",
    Map("tree" -> x1),
    // tree <= x -> (nil, nil)
    ("tree", Seq.empty,  SymbolicHeap(x1 -> (nil, nil))),
    // tree <= ∃ y z . x -> (y, z) * tree(y) * tree(z)
    ("tree", Seq("y", "z"), SymbolicHeap(x1 -> (y1, y2), P("tree")(y1), P("tree")(y2)))
  )

  lazy val Dll = SID("dll",
    "Doubly-linked list",
    Map("dll" -> x1),
    // dll <= emp : { a = c, b = d }
    ("dll", Seq.empty, SymbolicHeap(x1 =:= x3, x2 =:= x4)),
    // dll <= ∃ u . a -> (u,b) * dll(u,a,c,d)
    ("dll", Seq("u"), SymbolicHeap(x1 -> (y1, x2), P("dll")(y1, x1, x3, x4)))
  )

  lazy val OddList = SID("odd",
    "Lists of odd length",
    Map("odd" -> x1, "even" -> x2),
    ("odd", Seq("n"), SymbolicHeap(x1 -> y1, P("even")(y1, x2))),
    ("even", Seq("n"), SymbolicHeap(x1 -> y1, P("odd")(y1, x2))),
    ("even", Seq.empty, SymbolicHeap(x1 =:= x2))
  )

  lazy val Tll = SID("tll",
    "Tree with linked leaves",
    Map("tll" -> x1),
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(x1 =:= x2, x1 -> (nil,nil,x3))),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap(x1 -> (y1,y2,nil), P("tll")(y1, x2, y3), P("tll")(y2, y3, x3)))
  )

  lazy val TllAcycExtraNeq = SID("tll",
    "Acyclic tree with linked leaves, overspecified",
    Map("tll" -> x1),
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(x1 =:= x2, x1 =/= x3, x1 -> (nil,nil,x3))),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap(x1 =/= x3, x2 =/= x3, x1 -> (y1,y2,nil), P("tll")(y1, x2, y3), P("tll")(y2, y3, x3)))
  )

  lazy val TllAcyc = SID("tll",
    "Acyclic tree with linked leaves",
    Map("tll" -> x1),
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(x1 =:= x2, x1 =/= x3, x1 -> (nil,nil,x3))),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap(x1 =/= x3, x1 -> (y1,y2,nil), P("tll")(y1, x2, y3), P("tll")(y2, y3, x3)))
  )

  /**
    * For illustrating the iterative nature; no memory allocation
    */
  lazy val EmptyLinearPermuter = SID("a",
    "Non-Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(x1 =:= x2, P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2))),
    ("c", Seq.empty, SymbolicHeap(x1 =:= x2, P("d")(x2, x1))),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  /**
    * For illustrating the iterative nature; memory allocation
    */
  lazy val NonEmptyLinearPermuter = SID("a",
    "Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(x1 =:= x2, P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2))),
    ("c", Seq.empty, SymbolicHeap(x1 =:= x2, P("d")(x2, x1))),
    ("d", Seq.empty, SymbolicHeap(x1 -> x2))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter = SID("a",
    "Binary FV Permuter with Optional Allocation",
    ("a", Seq.empty, SymbolicHeap(x1 =:= x2, P("b")(x1, x2), P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter2 = SID("a",
    "Binary FV Permuter without Allocation",
    ("a", Seq.empty, SymbolicHeap(P("b")(x1, x2), P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("c", Seq.empty, SymbolicHeap(x1 =/= x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter3 = SID("a",
    "Binary FV Permuter with optional allocation and inequalities",
    ("a", Seq.empty, SymbolicHeap(P("b")(x1, x2), P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("c", Seq.empty, SymbolicHeap(x1 =/= x2))
  )

  lazy val UnsatSID = SID("unsat",
    "Unsat singly-linked list",
    ("unsat", Seq.empty, SymbolicHeap(x1 =/= x2, x1 -> nil, P("sll")(x1, x2))),
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(x1 =:= x2)),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

  lazy val UnsatSID2 = SID("unsat",
    "Tree with unsat leaves",
    ("unsat", Seq("y", "z"), SymbolicHeap(x1 -> (y1, y2), P("tree")(y1), P("tree")(y2))),
    ("tree", Seq("y", "z"), SymbolicHeap(x1 -> (y1, y2), P("tree")(y1), P("tree")(y2))),
    ("tree", Seq.empty, SymbolicHeap(nil =/= nil, x1 -> (nil, nil)))
  )

  lazy val OptionallyEstablishedSID = SID("start",
    "Optionally Established SID",
    ("start", Seq("z", "y"), SymbolicHeap(P("pred")(y1, x1), P("pred")(y2, x1))),
    ("pred", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("pred", Seq.empty, SymbolicHeap(x2 -> x1))
  )

  lazy val OptionallyEstablishedSID2 = SID("a",
    "Optionally Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(P("b")(y1, y2), P("b")(y1, y3), P("d")(y2, y1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("d", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val OptionallyEstablishedSID3 = SID("a",
    "Optionally Established SID 3",
    ("a", Seq("y","z"), SymbolicHeap(P("b")(y1, y2), P("b")(y2, y1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val OptionallyEstablishedSID4 = SID("a",
    "Optionally Established SID 4",
    ("a", Seq("y","z","w"), SymbolicHeap(P("b")(y3, y2), P("b")(y1, y2), P("d")(y2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("b", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("d", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val NonEstablishedSID = SID("start",
    "Non-Established SID",
    ("start", Seq("y","z"), SymbolicHeap(P("pred")(y1, x1, y2))),
    ("pred", Seq.empty, SymbolicHeap(Seq(x1,x2,x3), x1 -> x2))
  )

  lazy val NonEstablishedSID2 = SID("a",
    "Non-Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(P("b")(y1, y2), P("b")(y1, y3), P("d")(y2, x2))),
    ("b", Seq.empty, SymbolicHeap(P("c")(x1, x2))),
    ("c", Seq.empty, SymbolicHeap(x2 -> x1)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("d", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val CyclicSll = SID("cll",
    "Cyclic list",
    ("cll", Seq.empty, SymbolicHeap(P("sll")(x1, x1))),
    ("sll", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("sll", Seq("y"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

  lazy val GarbageSll = SID("cll",
    "Garbage list",
    ("sll", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("sll", Seq("y", "z"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

}
