package spritemodel

trait Removable extends HasRemovalMarker {
  protected var markedForRemoval: Boolean = false

  def markForRemoval: Unit = markedForRemoval = true

  override def isMarkedForRemoval: Boolean = markedForRemoval
}

trait HasRemovalMarker {
  def isMarkedForRemoval: Boolean
}
