package org.kcw
package sprite
import java.awt.Graphics2D

/**
  * Some Sprites do not have seperate images for their rotations, in that case, we'll need to
  * rely on the underlying Graphics to rotate the sprite's image using the graphics card for each
  * rendering pass.
  */
trait Rotation { self: Sprite ⇒

  /**
    * The sprite's current rotation in degrees
    */
  def rotation: Int

  override def paintEffect(g: Graphics2D): Unit = {
    val xx = x
    val yy = y
    spriteSheet.paint(g, currentSpriteTileId, xx, yy, rotation)
    outline.foreach(_.paint(g, xx, yy))
  }

}
