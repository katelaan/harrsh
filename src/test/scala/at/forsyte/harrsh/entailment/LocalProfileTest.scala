package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.ExampleSids._

class LocalProfileTest extends TestWithSyntacticSugarForDecompositions {
  
  property("Correctness of local profile computation") {

    val inputs = Table(
      ("lhs", "sid", "profile"),
      ("x1 -> x2", Nel, Set(
        D(Nel)(C(Seq(pr("nel", x1, x2)), alloc(x1)++ref(x2), Set(nil =/= x1))),
        D(Nel)(C(Seq(pr("nel", x1, p1), pr("nel", x2, p1)), alloc(x1)++ref(x2)++unused(p1), Set(nil =/= x1))))),
      // FIXME: Note that bound vars may occur in the profile, with the effect that we cannot simply construct the top-level predicate call by taking the first arity(pred) many parameters to construct the top-level call!
      // TODO: It would be better to get rid of this case by dropping the quantifiers prior to profile computation
      ("x1 -> y", Nel, Set(
        D(Nel)(C(Seq(pr("nel", x1, y1)), alloc(x1)++ref(y1), Set(nil =/= x1))),
        D(Nel)(C(Seq(pr("nel", x1, p1), pr("nel", y1, p1)), alloc(x1)++ref(y1)++unused(p1), Set(nil =/= x1))))),
      ("x3 -> x1", Nel, Set(
        D(Nel)(C(Seq(pr("nel", x3, x1)), alloc(x3)++ref(x1), Set(nil =/= x3))),
        D(Nel)(C(Seq(pr("nel", x3, p1), pr("nel", x1, p1)), alloc(x3)++ref(x1)++unused(p1), Set(nil =/= x3))))),
      ("x1 -> (nil, nil)", Tree, Set(
        D(Tree)(C(Seq(pr("tree", x1)), alloc(x1), Set(nil =/= x1)))
      )),
      ("x1 -> (x2, x3)", Tree, Set(
        D(Tree)(C(Seq(pr("tree", x1)), alloc(x1), Set(nil =/= x1), missing = Set(nil =:= x3,x2 =:= x3,x1 =/= x2,x1 =/= x3,nil =:= x2))),
        D(Tree)(C(Seq(pr("tree", x1), pr("tree", x2), pr("tree", x3)), alloc(x1)++ref(x2,x3), Set(nil =/= x1)))
      )),
      ("x1 -> x2", MixedAritySll, Set(
        D(MixedAritySll)(C(Seq(pr("mixedarity", x1, x2)), alloc(x1)++ref(x2), Set(nil =/= x1))),
        D(MixedAritySll)(C(Seq(pr("mixedarity", x1, p1), pr("mixedarity", x2, p1)), alloc(x1)++ref(x2)++unused(p1), Set(nil =/= x1))))
      ),
      // TODO: Shouldn't we record the equality x1 = x3? It is implicit in the repetition in the parameter list, but still?
      ("x1 -> (x4, x2)", NeDllEq, Set(
        D(NeDllEq)(C(Seq(pr("dll", x1, x2, x1, x4)), alloc(x1)++ref(x2,x4), Set(nil =/= x1))),
        D(NeDllEq)(C(Seq(pr("dll", x1, x2, p1, p2), pr("dll", x4, x1, p1, p2)), alloc(x1)++ref(x2,x4)++unused(p1,p2), Set(nil =/= x1)))
      )),
      ("x1 -> (x2, null)", Nel, Set.empty),
      // There should only be a single TLL-profile for a single linked leaf,
      // because all other decompositions would involve having an unused root parameter for the recursive call
      // If we ever decide to relax the rootedness assumptions, this test case might break
      ("x1 -> (null, null, x2)", Tll, Set(
        D(Tll)(C(Seq(pr("tll", x1, x1, x2)), alloc(x1)++ref(x2), Set(nil =/= x1)))
      ))
    )


    for {
      (lhs, sid, expectedProfile) <- inputs
    } {
      info(s"Computing profile for $lhs wrt. $sid using new approach")
      val actualProfile = LocalProfile.decompsOfNonemptyLocalAllocation(lhs.parse, sid)
      actualProfile shouldEqual expectedProfile
    }


  }

}
