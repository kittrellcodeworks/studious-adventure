package org.kcw.sprite

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
trait Sprite extends GraphicEffect {

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

  /**
    * Paints the Sprite's current tile to the target graphics using the specified translation/scaling transform
    *
    * @param g the target graphics
    */
  def paintEffect(g: Graphics2D): Unit = {
    val xx = x
    val yy = y
    spriteSheet.paint(g, currentSpriteTileId, xx, yy)
    outline.foreach(_.paint(g, xx, yy))
  }

  def currentSpriteTileId_=(tileId: Int): Unit =
    mCurTileID = if (tileId >= spriteSheet.size && spriteSheet.nonEmpty) tileId % spriteSheet.size else tileId
}
