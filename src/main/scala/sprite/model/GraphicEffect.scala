package sprite.model

import java.awt.Graphics2D
import java.awt.image.AffineTransformOp

trait GraphicEffect extends HasRemovalMarker {
  /**
    * Paints the Sprite's current tile to the target graphics using the specified translation/scaling transform
    *
    * @param g the target graphics - will already have scale or translations applied.
    */
  def paint(g: Graphics2D): Unit
}
