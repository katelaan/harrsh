package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.main.FV

/**
  * Created by jkatelaa on 10/17/16.
  */
case class MapBasedRenaming(map : Map[FV, FV]) extends Renaming {

  override lazy val codomain: Set[FV] = map.values.toSet

  override def apply(x: FV): FV = map.getOrElse(x, x)

  override def extendWith(k: FV, v: FV): Renaming = MapBasedRenaming(map + (k -> v))
}
