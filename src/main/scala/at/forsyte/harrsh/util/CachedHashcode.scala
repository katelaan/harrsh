package at.forsyte.harrsh.util

trait CachedHashcode {

  self: Product =>

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}
