package at.forsyte.harrsh.util

import at.forsyte.harrsh.main.HarrshLogging

object CacheRegistry extends HarrshLogging {

  private var registry: Set[HarrshCache[_,_]] = Set.empty

  def summary: String = {
    registry.map(_.summary).mkString("CacheRegistry(\n  ", ",\n  ", ")")
  }

  def register(value: HarrshCache[_, _]): Unit = {
    registry += value
  }

  def resetAllCaches(): Unit = {
    logger.debug("Will reset all registered caches,\n" + summary)
    registry foreach (_.reset())
  }

}
