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

  they should "generate all permutations" in {

    val input = Seq(1,2,3,4)
    val permutations = Combinators.permutations(input)
    assert(permutations.size == 4*3*2*1)
    for {
      i <- 1 to 4
      j <- 1 to 4
      k <- 1 to 4
      l <- 1 to 4
      if i != j && i != k && i != l && j != k && j != l && k != l
    }{
      assert(permutations.contains(Seq(i,j,k,l)))
    }

  }

}
