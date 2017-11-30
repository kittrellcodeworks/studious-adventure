package spritemodel

import java.awt.Graphics2D

import com.robowar.physics.Point

class Sprite(spriteSheet: SpriteSheet, initialPos: Point) extends GraphicEffect with Removable {

  var pos: Point = initialPos

  private var mCurFaceID: Int = 0
  private var mOutlineOval: Int = 0

  // TODO: scaling?
  def paint(g: Graphics2D): Unit = {
    val x = pos.x.toInt
    val y = pos.y.toInt
    spriteSheet.paint(g, mCurFaceID, x, y)

    if (mOutlineOval > 0)
      g.drawOval(x - mOutlineOval / 2, y - mOutlineOval / 2, mOutlineOval, mOutlineOval)
  }

  def outlineOval(paramInt: Int): Unit = mOutlineOval = paramInt

  def width: Int = spriteSheet.tileWidth

  def height: Int = spriteSheet.tileHeight

  def currentFaceID: Int = mCurFaceID

  def currentFaceID_=(faceId: Int): Unit =
    mCurFaceID = if (faceId >= spriteSheet.size) faceId % spriteSheet.size else faceId
}
