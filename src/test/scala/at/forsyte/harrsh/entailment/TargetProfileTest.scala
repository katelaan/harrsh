package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.ExampleSids._

class TargetProfileTest extends TestWithSyntacticSugarForDecompositions {

  property("Correctness of local profile computation") {

    val inputs = Table(
      ("lhs", "sid", "sources", "target"),
      // Starting from a profile of x1 -> x2, we get the (identical) profile for a longer NEL
      // TODO: Change tests to work with new contexts/decompositions
//      ("x1 -> y * nel(y, x2)", Nel, Seq(
//        EntailmentProfile(Set(
//          D(Nel)(C(Seq(pr("nel", x1, x2)), alloc(x1) ++ ref(x2), Set(nil =/= x1))),
//          D(Nel)(C(Seq(pr("nel", x1, p1), pr("nel", x2, p1)), alloc(x1) ++ ref(x2) ++ unused(p1), Set(nil =/= x1)))),
//          Seq(x1,x2)
//        )),
//        EntailmentProfile(Set(
//          D(Nel)(C(Seq(pr("nel", x1, x2)), alloc(x1) ++ ref(x2), Set(nil =/= x1))),
//          D(Nel)(C(Seq(pr("nel", x1, p1), pr("nel", x2, p1)), alloc(x1) ++ ref(x2) ++ unused(p1), Set(nil =/= x1)))),
//          Seq(x1,x2)
//        )
//      ),
//      // Empty profile if there's no consistent local allocation
//      ("x1 -> (x2, null) * nel(y, x2)", Nel, Seq(
//        EntailmentProfile(Set(
//          D(Nel)(C(Seq(pr("nel", x1, x2)), alloc(x1) ++ ref(x2), Set(nil =/= x1))),
//          D(Nel)(C(Seq(pr("nel", x1, p1), pr("nel", x2, p1)), alloc(x1) ++ ref(x2) ++ unused(p1), Set(nil =/= x1)))),
//          Seq(x1,x2)
//        )),
//        EntailmentProfile(Set.empty, Seq(x1,x2))
//      ),
//
//      // Starting from a profile of a single tll leaf pointer, if we read another leaf pointer we get a decomposition with two parts
//      ("x1 -> (null, null, x2) * oneptr(x2, x3)", Tll, Seq(
//        EntailmentProfile(Set(
//          D(Tll)(C(Seq(pr("tll", x1, x1, x2)), alloc(x1)++ref(x2), Set(nil =/= x1)))),
//          Seq(x1,x2)
//        )),
//        EntailmentProfile(Set(
//          D(Tll)(
//            C(Seq(pr("tll", x1, x1, x2)), alloc(x1)++ref(x2), Set(nil =/= x1)),
//            C(Seq(pr("tll", x2, x2, x3)), alloc(x2)++ref(x3), Set(nil =/= x2))
//          )
//        ), Seq(x1,x2,x3))
//      )
    )

    for {
      (lhs, sid, sourceProfiles, expectedProfile) <- inputs
    } {
//      info(s"Computing target profile for $lhs wrt. $sid from sources ${sourceProfiles.mkString(",\n")}")
//      val actualProfile = TargetProfile(sourceProfiles, lhs.parse, sid).get.get
//      info(s"Computed target profile: $actualProfile")
//      actualProfile shouldEqual expectedProfile
    }
  }

}
