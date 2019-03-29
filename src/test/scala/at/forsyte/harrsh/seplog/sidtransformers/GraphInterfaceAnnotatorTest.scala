package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.{ExampleSids, TestValues}
import at.forsyte.harrsh.test.HarrshTableTest

class GraphInterfaceAnnotatorTest extends HarrshTableTest with TestValues {

  val inputs = Table(
    ("sid", "pred", "alloced", "refed", "root", "sink", "classes"),
    (ExampleSids.Nel, ExampleSids.Nel.startPred, Set(x1), Set(x2), Set(x1), Set.empty, Set.empty),
    (ExampleSids.Sll, ExampleSids.Sll.startPred, Set(x1), Set(x2), Set(x1), Set.empty, Set.empty),
    (ExampleSids.NoProgressSll, ExampleSids.NoProgressSll.startPred, Set(x1), Set(x2), Set(x1), Set(x2), Set.empty),
    (ExampleSids.Tll, ExampleSids.Tll.startPred, Set(x1, x2), Set(nil,x3), Set(x1), Set.empty, Set.empty)
  )

  property("Computing graph interface of SID") {
    forAll(inputs) {
      (sid, pred, alloced, refed, root, sink, classes) =>
        val ifMap = GraphInterfaceAnnotator(sid)
        val gif = ifMap(pred)
        info(gif.toString)
        gif.alloced shouldEqual alloced
        gif.refed shouldEqual refed
        gif.root shouldEqual root
        gif.sink shouldEqual sink
        gif.equivalenceClasses shouldEqual classes
    }
  }

}
