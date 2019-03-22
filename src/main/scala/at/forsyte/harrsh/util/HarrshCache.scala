package at.forsyte.harrsh.util

import at.forsyte.harrsh.main.HarrshLogging

import scala.collection.mutable

sealed trait HarrshCache[From,To] extends HarrshLogging {

  val description: String

  def reset()

  def apply(from: From): To

  def summary: String

}

class UnboundedCache[From,CacheKey,To](override val description: String, toCacheKey: From => CacheKey, computeResult: From => To) extends HarrshCache[From, To] {

  CacheRegistry.register(this)

  private val cache: mutable.Map[CacheKey, To] = mutable.Map.empty

  private var hits: Int = 0
  private var misses: Int = 0
  def stats: String = s"[Hits: $hits; Misses: $misses; Hit rate: ${hits / Math.max(1, hits+misses).toDouble}]"

  override def apply(from: From): To = {
    val key = toCacheKey(from)
    if (cache.isDefinedAt(key)) {
      hits += 1
      logger.debug(s"Cache hit for $key => Skip computation $stats")
      cache(key)
    } else {
      misses += 1
      logger.debug(s"Cache miss => compute result $stats")
      val value = computeResult(from)
      cache.update(key, value)
      value
    }
  }

  override def reset(): Unit = {
    cache.clear()
  }

  override def summary: String = description + " " + stats

}

class DeactivatedCache[From,CacheKey,To](override val description: String, toCacheKey: From => CacheKey, computeResult: From => To) extends HarrshCache[From, To] {

  override def apply(from: From): To = {
    computeResult(from)
  }

  override def reset(): Unit = {
    // Nothing to do, as nothing is cached
  }

  override def summary: String = description + " (deactivated)"
}