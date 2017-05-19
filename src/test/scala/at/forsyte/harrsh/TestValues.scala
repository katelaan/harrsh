package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.Var

/**
  * Created by jens on 4/7/17.
  */
trait TestValues {

  val (x1,x2,x3,x4,x5,x6) = (Var(1),Var(2),Var(3),Var(4),Var(5),Var(6))
  val (y1,y2,y3,y4,y5,y6,y7,y8,y9) = (Var(-1),Var(-2),Var(-3),Var(-4),Var(-5),Var(-6),Var(-7),Var(-8),Var(-9))

}
