package sprite.model

import java.awt.Graphics2D

/**
  * A Sprite is a mutable representation of the location and currently active sprite-sheet tile for a graphically
  * representable object within a game.
  *
  * The sprite's position is intended to be relative to it's world's coordinate system, and expects to be told how to
  * translate those coordinates into screen pixels with an AffineTransform during the painting cycle.
  *
  *
  */
trait Sprite extends GraphicEffect with Removable {

  private var mCurTileID = 0

  /**
    * the sprite's current unscaled center x-coordinate in its parent's coordinate system
    */
  def x: Int

  /**
    * the sprite's current unscaled center y-coordinate in its parent's coordinate system
    */
  def y: Int

  def spriteSheet: SpriteSheet = SpriteSheet.empty
  def currentSpriteTileId: Int = mCurTileID
  def outline: Option[OutlineCircle] = None
  def width: Int = spriteSheet.tileWidth
  def height: Int = spriteSheet.tileHeight

  /**
    * Paints the Sprite's current tile to the target graphics using the specified translation/scaling transform
    *
    * @param g the target graphics
    * @param tx a transform describing the current view of the sprite's containing coordinate system (the world)
    */
  def paint(g: Graphics2D): Unit = {
    spriteSheet.paint(g, currentSpriteTileId, x, y)
    outline.foreach(_.paint(g, x, y))
  }

  def currentSpriteTileId_=(tileId: Int): Unit =
    mCurTileID = if (tileId >= spriteSheet.size && spriteSheet.nonEmpty) tileId % spriteSheet.size else tileId
}
