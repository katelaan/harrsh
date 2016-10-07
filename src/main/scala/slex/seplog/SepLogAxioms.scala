package slex.seplog

/**
  * Created by jkatelaa on 9/30/16.
  */
object SepLogAxioms {

  val LSegDef = SepLogDef(
    //lseg(x, z, n) =
    IxLSeg("x", "z", "n"),
    Or(
      // (x = z ∧ n != 0 ∧ emp)
      And(PtrEq("x", "z"),
          And(IxEq("n", 0),
          Emp())),
      //∨ (x != z ∧ n > 0 ∧ ∃y. next(x, y) ∗ lseg(y, z, n − 1))
      And(PtrNEq("x", "z"),
        And(IxGT("n", 0),
          Exists("y", SepCon(PointsTo("x", "y"),
                      IxLSeg("y", "z", Minus("n", 1)) )))
      )
    )
  )

}
