package at.forsyte.harrsh.heapautomata.utils

/**
  * Created by jens on 3/29/17.
  */
trait StateTag[A] {

  def valsOfTag : Set[A]
  val inconsistentTag : A
  def isFinalTag(tag : A) : Boolean

}

object StateTag {

  object instances {

    val booleanTag = new StateTag[Boolean] {
      override def valsOfTag: Set[Boolean] = Set(true, false)

      // An inconsistent heap always has all desired properties (garbage-freedom, acyclicity...)
      override val inconsistentTag: Boolean = true

      override def isFinalTag(tag: Boolean): Boolean = tag
    }

  }

}