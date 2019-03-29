package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.inductive.{RichSid, Sid, SidFactory, SymbolicHeap}

/**
  * Created by jens on 10/15/16.
  */
object ExampleSids extends TestValues {
  
  lazy val Sll: RichSid = SidFactory.makeRootedSid("sll",
    "Singly-linked list",
    Map("sll" -> x1),
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(x1 =:= x2)),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

  lazy val Nel: RichSid = SidFactory.makeRootedSid("nel",
    "Nonempty list",
    Map("nel" -> x1),
    // nel <= emp : { a = b }
    ("nel", Seq.empty, SymbolicHeap(x1 -> x2)),
    // nel <= ∃ y . a -> y * nel(y, b)
    ("nel", Seq("y"), SymbolicHeap(x1 -> y1, P("nel")(y1, x2)))
  )

  lazy val AcycNel: RichSid = SidFactory.makeRootedSid("anel",
    "Acyclic nonempty list",
    Map("anel" -> x1),
    // nel <= emp : { a = b }
    ("anel", Seq.empty, SymbolicHeap(x1 -> x2, x1 =/= x2)),
    // nel <= ∃ y . a -> y * nel(y, b)
    ("anel", Seq("y"), SymbolicHeap(x1 -> y1, x1 =/= x2, P("anel")(y1, x2)))
  )

  lazy val NoProgressSll: RichSid = SidFactory.makeRootedSid("sll",
    "Singly-linked list",
    Map("sll" -> x1),
    ("sll", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("sll", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("sll", Seq("y"), SymbolicHeap(P("sll")(x1, y1), P("sll")(y1, x2)))
  )

  lazy val Tree: RichSid = SidFactory.makeRootedSid("tree",
    "Null-terminated tree",
    Map("tree" -> x1),
    // tree <= x -> (nil, nil)
    ("tree", Seq.empty,  SymbolicHeap(x1 -> (nil, nil))),
    // tree <= ∃ y z . x -> (y, z) * tree(y) * tree(z)
    ("tree", Seq("y", "z"), SymbolicHeap(x1 -> (y1, y2), P("tree")(y1), P("tree")(y2)))
  )

  lazy val Dll: RichSid = SidFactory.makeRootedSid("dll",
    "Doubly-linked list",
    Map("dll" -> x1),
    // dll <= emp : { a = c, b = d }
    ("dll", Seq.empty, SymbolicHeap(x1 =:= x3, x2 =:= x4)),
    // dll <= ∃ u . a -> (u,b) * dll(u,a,c,d)
    ("dll", Seq("u"), SymbolicHeap(x1 -> (y1, x2), P("dll")(y1, x1, x3, x4)))
  )

  lazy val NeDll: RichSid = SidFactory.makeRootedSid("dll",
    "Nonempty doubly-linked list",
    Map("dll" -> x1),
    ("dll", Seq.empty, SymbolicHeap(x1 -> (x3, x2))),
    ("dll", Seq("u"), SymbolicHeap(x1 -> (y1, x2), P("dll")(y1, x1, x3)))
  )

  lazy val NeDllEq: RichSid = SidFactory.makeRootedSid("dll",
    "Nonempty doubly-linked list with equality in base case",
    Map("dll" -> x1),
    ("dll", Seq.empty, SymbolicHeap(x1 -> (x4, x2), x1 =:= x3)),
    ("dll", Seq("y"), SymbolicHeap(x1 -> (y1, x2), P("dll")(y1, x1, x3, x4)))
  )

  lazy val NeAcycDll: RichSid = SidFactory.makeRootedSid("dll",
    "Nonempty acyclic doubly-linked list",
    Map("dll" -> x1),
    ("dll", Seq.empty, SymbolicHeap(x1 -> (x4, x2), x1 =:= x3, x1 =/= x4)),
    ("dll", Seq("y"), SymbolicHeap(x1 -> (y1, x2), P("dll")(y1, x1, x3, x4), x1 =/= x2, x1 =/= x4))
  )

  lazy val OddList: RichSid = SidFactory.makeRootedSid("odd",
    "Lists of odd length",
    Map("odd" -> x1, "even" -> x1),
    ("odd", Seq("n"), SymbolicHeap(x1 -> y1, P("even")(y1, x2))),
    ("even", Seq("n"), SymbolicHeap(x1 -> y1, P("odd")(y1, x2))),
    ("even", Seq.empty, SymbolicHeap(x1 =:= x2))
  )

  lazy val OddNel: RichSid = SidFactory.makeRootedSid("odd",
    "Nonempty lists of odd length",
    Map("odd" -> x1, "even" -> x1),
    ("odd", Seq("n"), SymbolicHeap(x1 -> x2)),
    ("odd", Seq("n"), SymbolicHeap(x1 -> y1, P("even")(y1, x2))),
    ("even", Seq("n"), SymbolicHeap(x1 -> y1, P("odd")(y1, x2))),
  )

  lazy val EvenNel: RichSid = SidFactory.makeRootedSid("even",
    "Nonempty lists of even length",
    Map("odd" -> x1, "even" -> x1),
    ("odd", Seq("n"), SymbolicHeap(x1 -> x2)),
    ("odd", Seq("n"), SymbolicHeap(x1 -> y1, P("even")(y1, x2))),
    ("even", Seq("n"), SymbolicHeap(x1 -> y1, P("odd")(y1, x2))),
  )

  lazy val Tll: RichSid = SidFactory.makeRootedSid("tll",
    "Tree with linked leaves",
    Map("tll" -> x1),
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(x1 =:= x2, x1 -> (nil,nil,x3))),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap(x1 -> (y1,y2,nil), P("tll")(y1, x2, y3), P("tll")(y2, y3, x3)))
  )

  lazy val TllAcycExtraNeq: RichSid = SidFactory.makeRootedSid("tll",
    "Acyclic tree with linked leaves, overspecified",
    Map("tll" -> x1),
    // tll <= a → (nil nil c) : { a = b }
    ("tll", Seq.empty, SymbolicHeap(x1 =:= x2, x1 =/= x3, x1 -> (nil,nil,x3))),
    // tll <= ∃ l r z . a → (l r nil) ∗ tll(l b z) ∗ tll(r z c)
    ("tll", Seq("l","r","z"), SymbolicHeap(x1 =/= x3, x2 =/= x3, x1 -> (y1,y2,nil), P("tll")(y1, x2, y3), P("tll")(y2, y3, x3)))
  )

  lazy val TllAcyc: RichSid = SidFactory.makeRootedSid("tll",
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
  lazy val EmptyLinearPermuter: Sid = SidFactory.makeSid("a",
    "Non-Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(x1 =:= x2, P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2))),
    ("c", Seq.empty, SymbolicHeap(x1 =:= x2, P("d")(x2, x1))),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  /**
    * For illustrating the iterative nature; memory allocation
    */
  lazy val NonEmptyLinearPermuter: Sid = SidFactory.makeSid("a",
    "Allocating Linear FV-Permuter",
    ("a", Seq.empty, SymbolicHeap(x1 =:= x2, P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2))),
    ("c", Seq.empty, SymbolicHeap(x1 =:= x2, P("d")(x2, x1))),
    ("d", Seq.empty, SymbolicHeap(x1 -> x2))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter: Sid = SidFactory.makeSid("a",
    "Binary FV Permuter with Optional Allocation",
    ("a", Seq.empty, SymbolicHeap(x1 =:= x2, P("b")(x1, x2), P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  /**
    * For illustrating the iterative nature with branching, as well as possible inconsistency
    */
  lazy val NonEmptyBinaryPermuter2: Sid = SidFactory.makeSid("a",
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
  lazy val NonEmptyBinaryPermuter3: Sid = SidFactory.makeSid("a",
    "Binary FV Permuter with optional allocation and inequalities",
    ("a", Seq.empty, SymbolicHeap(P("b")(x1, x2), P("b")(x2, x1))),
    ("b", Seq.empty, SymbolicHeap(P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("c", Seq.empty, SymbolicHeap(x1 =/= x2))
  )

  lazy val UnsatSID: Sid = SidFactory.makeSid("unsat",
    "Unsat singly-linked list",
    ("unsat", Seq.empty, SymbolicHeap(x1 =/= x2, x1 -> nil, P("sll")(x1, x2))),
    // sll <= emp : { a = b }
    ("sll", Seq.empty, SymbolicHeap(x1 =:= x2)),
    // sll <= ∃ y . a -> y * sll(y, b)
    ("sll", Seq("y"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

  lazy val UnsatSID2: Sid = SidFactory.makeSid("unsat",
    "Tree with unsat leaves",
    ("unsat", Seq("y", "z"), SymbolicHeap(x1 -> (y1, y2), P("tree")(y1), P("tree")(y2))),
    ("tree", Seq("y", "z"), SymbolicHeap(x1 -> (y1, y2), P("tree")(y1), P("tree")(y2))),
    ("tree", Seq.empty, SymbolicHeap(nil =/= nil, x1 -> (nil, nil)))
  )

  lazy val OptionallyEstablishedSID: Sid = SidFactory.makeSid("start",
    "Optionally Established SID",
    ("start", Seq("z", "y"), SymbolicHeap(P("pred")(y1, x1), P("pred")(y2, x1))),
    ("pred", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("pred", Seq.empty, SymbolicHeap(x2 -> x1))
  )

  lazy val OptionallyEstablishedSID2: Sid = SidFactory.makeSid("a",
    "Optionally Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(P("b")(y1, y2), P("b")(y1, y3), P("d")(y2, y1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("d", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val OptionallyEstablishedSID3: Sid = SidFactory.makeSid("a",
    "Optionally Established SID 3",
    ("a", Seq("y","z"), SymbolicHeap(P("b")(y1, y2), P("b")(y2, y1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2, P("c")(x1, x2), P("c")(x2, x1))),
    ("c", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val OptionallyEstablishedSID4: Sid = SidFactory.makeSid("a",
    "Optionally Established SID 4",
    ("a", Seq("y","z","w"), SymbolicHeap(P("b")(y3, y2), P("b")(y1, y2), P("d")(y2, x1))),
    ("b", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("b", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("d", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val NonEstablishedSID: Sid = SidFactory.makeSid("start",
    "Non-Established SID",
    ("start", Seq("y","z"), SymbolicHeap(P("pred")(y1, x1, y2))),
    ("pred", Seq.empty, SymbolicHeap(Seq(x1,x2,x3), x1 -> x2))
  )

  lazy val NonEstablishedSID2: Sid = SidFactory.makeSid("a",
    "Non-Established SID 2",
    ("a", Seq("y","z","w"), SymbolicHeap(P("b")(y1, y2), P("b")(y1, y3), P("d")(y2, x2))),
    ("b", Seq.empty, SymbolicHeap(P("c")(x1, x2))),
    ("c", Seq.empty, SymbolicHeap(x2 -> x1)),
    ("c", Seq.empty, SymbolicHeap(Seq(x1,x2))),
    ("d", Seq.empty, SymbolicHeap(x1 =:= x2)),
    ("d", Seq.empty, SymbolicHeap(Seq(x1,x2)))
  )

  lazy val CyclicSll: Sid = SidFactory.makeSid("cll",
    "Cyclic list",
    ("cll", Seq.empty, SymbolicHeap(P("sll")(x1, x1))),
    ("sll", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("sll", Seq("y"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

  lazy val GarbageSll: Sid = SidFactory.makeSid("cll",
    "Garbage list",
    ("sll", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("sll", Seq("y", "z"), SymbolicHeap(x1 -> y1, P("sll")(y1, x2)))
  )

  lazy val MixedAritySll: RichSid = SidFactory.makeRootedSid("mixedarity",
    "Mixed Arity SLL",
    Map("mixedarity" -> x1),
    ("mixedarity", Seq.empty, SymbolicHeap(x1 -> x2)),
    ("mixedarity", Seq.empty, SymbolicHeap(x1 -> (x2,nil))),
    ("mixedarity", Seq("y"), SymbolicHeap(x1 -> y1, P("mixedarity")(y1, x2))),
    ("mixedarity", Seq("y"), SymbolicHeap(x1 -> (y1,nil), P("mixedarity")(y1, x2)))
  )

}
