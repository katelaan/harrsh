package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.Implicits._
import at.forsyte.harrsh.test.HarrshTest

/**
  * Created by jkatelaa on 4/20/17.
  */
class SIDUnfoldingTest extends HarrshTest {

  behavior of "The SID unfolding"

  it should "return a reduced heap" in {

    SIDUnfolding.firstReducedUnfolding(refinedSid)

  }

  val refinedSid : SID =
    """    P(x1, x2) <= P4(x1,x2) ;
      |    P0(x1, x2) <= one17(x1) * one17(x2) * Q0(x1,x2) ;
      |    P0(x1, x2) <= one17(x1) * one17(x2) * Q10(x1,x2) ;
      |    P0(x1, x2) <= one17(x1) * one17(x2) * Q9(x1,x2) ;
      |    P0(x1, x2) <= one17(x1) * one17(x2) * Q11(x1,x2) ;
      |    P4(x1, x2) <= one17(x1) * one17(x2) * Q4(x1,x2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit0(y1,y2,x1,x2) * Q10(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit0(y1,y2,x1,x2) * Q9(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit0(y1,y2,x1,x2) * Q11(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit15(y1,y2,x1,x2) * Q4(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit3(y1,y2,x1,x2) * Q4(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit8(y1,y2,x1,x2) * Q0(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit3(y1,y2,x1,x2) * Q0(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit0(y1,y2,x1,x2) * Q4(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit15(y1,y2,x1,x2) * Q0(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit7(y1,y2,x1,x2) * Q4(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit7(y1,y2,x1,x2) * Q10(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit7(y1,y2,x1,x2) * Q11(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit15(y1,y2,x1,x2) * Q10(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit15(y1,y2,x1,x2) * Q9(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit8(y1,y2,x1,x2) * Q10(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit8(y1,y2,x1,x2) * Q9(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit8(y1,y2,x1,x2) * Q11(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit3(y1,y2,x1,x2) * Q9(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit3(y1,y2,x1,x2) * Q11(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit0(y1,y2,x1,x2) * Q0(y1,y2) ;
      |    Q0(x1, x2) <= ∃y1 ∃y2 . succ2circuit7(y1,y2,x1,x2) * Q0(y1,y2) ;
      |    Q10(x1, x2) <= zero16(x1) * zero16(x2) ;
      |    Q10(x1, x2) <= ∃y1 ∃y2 . succ2circuit8(y1,y2,x1,x2) * Q4(y1,y2) ;
      |    Q11(x1, x2) <= ∃y1 ∃y2 . succ2circuit7(y1,y2,x1,x2) * Q9(y1,y2) ;
      |    Q4(x1, x2) <= ∃y1 ∃y2 . succ2circuit15(y1,y2,x1,x2) * Q11(y1,y2) ;
      |    Q9(x1, x2) <= ∃y1 ∃y2 . succ2circuit3(y1,y2,x1,x2) * Q10(y1,y2) ;
      |    and2(x1, x2, x3) <= zero16(x1) * zero16(x3) ;
      |    and5(x1, x2, x3) <= zero16(x2) * zero16(x3) ;
      |    and6(x1, x2, x3) <= one17(x1) * one17(x2) * one17(x3) ;
      |    not11(x1, x2) <= zero16(x1) * one17(x2) ;
      |    not9(x1, x2) <= one17(x1) * zero16(x2) ;
      |    one17(x1) <= emp : {x1 ≉ null} ;
      |    succ2circuit0(x1, x2, x3, x4) <= not11(x1,x3) * xor12(x1,x2,x4) ;
      |    succ2circuit0(x1, x2, x3, x4) <= not11(x1,x3) * xor13(x1,x2,x4) ;
      |    succ2circuit0(x1, x2, x3, x4) <= not9(x1,x3) * xor1(x1,x2,x4) ;
      |    succ2circuit0(x1, x2, x3, x4) <= not9(x1,x3) * xor14(x1,x2,x4) ;
      |    succ2circuit15(x1, x2, x3, x4) <= not11(x1,x3) * xor14(x1,x2,x4) ;
      |    succ2circuit3(x1, x2, x3, x4) <= not11(x1,x3) * xor1(x1,x2,x4) ;
      |    succ2circuit7(x1, x2, x3, x4) <= not9(x1,x3) * xor12(x1,x2,x4) ;
      |    succ2circuit8(x1, x2, x3, x4) <= not9(x1,x3) * xor13(x1,x2,x4) ;
      |    xor1(x1, x2, x3) <= zero16(x1) * zero16(x2) * zero16(x3) ;
      |    xor12(x1, x2, x3) <= one17(x1) * zero16(x2) * one17(x3) ;
      |    xor13(x1, x2, x3) <= one17(x1) * one17(x2) * zero16(x3) ;
      |    xor14(x1, x2, x3) <= zero16(x1) * one17(x2) * one17(x3) ;
      |    zero16(x1) <= emp : {x1 ≈ null}""".stripMargin.parseSID

}
