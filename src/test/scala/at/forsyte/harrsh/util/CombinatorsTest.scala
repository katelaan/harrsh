package at.forsyte.harrsh.util

import at.forsyte.harrsh.test.HarrshTest

/**
  * Created by jens on 4/25/17.
  */
class CombinatorsTest extends HarrshTest {

  behavior of "Combinators"

  they should "generate all partitions" in {

    val input = Set(1,2,3,4)
    val partitions = Combinators.partitions(input)

    assert(partitions.size == Math.pow(2,input.size).toInt)
    for {
      set <- input.subsets()
    } {
      val complement = input diff set
      assert(partitions.contains((set,complement)))
    }

  }

}
