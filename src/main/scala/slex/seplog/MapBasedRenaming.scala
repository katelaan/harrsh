package slex.seplog

/**
  * Created by jkatelaa on 10/17/16.
  */
case class MapBasedRenaming(map : Map[String, String]) extends Renaming {

  override lazy val codomain: Set[String] = map.values.toSet

  override def apply(s: String): String = map.getOrElse(s, s)

  override def extendWith(k: String, v: String): Renaming = MapBasedRenaming(map + (k -> v))
}
