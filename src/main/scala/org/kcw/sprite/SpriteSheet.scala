package org.kcw
package sprite

import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}

case class SpriteSheet(name: String, tileWidth: Int, tileHeight: Int, tiles: IndexedSeq[BufferedImage]) {
  private val halfWidth = tileWidth / 2
  private val halfHeight = tileHeight / 2

  def isEmpty: Boolean = tiles.isEmpty

  def nonEmpty: Boolean = tiles.nonEmpty

  def size: Int = tiles.size

  def paint(g: Graphics2D, tile: Int, x: Int, y: Int): Unit = if (tiles.isDefinedAt(tile)) {
    g.drawImage(tiles(tile), x - halfWidth, y - halfHeight, null)
  }

  def paint(g: Graphics2D, tile: Int, x: Int, y: Int, rot: Int): Unit = if (tiles.isDefinedAt(tile)) {
    val r = if (rot < 0) (rot % 360) + 360 else rot % 360
    val sin = precalc unsafe_sin r
    val cos = precalc unsafe_cos r
    val minusCos = -cos
    val op = new AffineTransformOp(new AffineTransform(
      // This will translate the image to center over the passed-in x and y, then rotate the specified degrees.
      /* m00 */ cos, /* m10 */ sin,
      /* m01 */ -sin, /* m11 */ cos,
      /* m02 */ halfWidth * minusCos + halfHeight * sin, /* m12 */ halfHeight * minusCos - halfWidth * sin
    ), AffineTransformOp.TYPE_BICUBIC) // for now, make it pretty... this can be reduced to BILINEAR if necessary
    g.drawImage(tiles(tile), op, x, y)
  }
}

object SpriteSheet {
  val empty: SpriteSheet = new SpriteSheet("", 0, 0, IndexedSeq.empty) {
    override def paint(g: Graphics2D, tile: Int, x: Int, y: Int): Unit = { }
  }

  def apply(name: String, srcImage: BufferedImage, tileCount: Dimension[Int] = IDim(1,1), count: Int = Int.MaxValue,
            imageSize: Dimension[Int] = IDim(0,0), offset: Point[Int] = IPoint(0,0), spacing: Dimension[Int] = IDim(0,0)
           ): SpriteSheet = {
    val tileWidth: Int = ((if (imageSize.w > 0) imageSize.w else (srcImage.getWidth - offset.x)) - ((tileCount.w - 1) * spacing.w)) / tileCount.w
    val tileHeight: Int = ((if (imageSize.h > 0) imageSize.h else (srcImage.getHeight - offset.y)) - ((tileCount.h - 1) * spacing.h)) / tileCount.h

    val cellWidth = tileWidth + spacing.w
    val cellHeight = tileHeight + spacing.h

    val max: Int = math.min(count, tileCount.w * tileCount.h)

    val tiles = for {
      k ← 0 until tileCount.h
      m ← 0 until tileCount.w
      if k * tileCount.w + m < max
    } yield srcImage.getSubimage(offset.x + (m * cellWidth), offset.y + (k * cellHeight), tileWidth, tileHeight)

    SpriteSheet(name, tileWidth, tileHeight, tiles)
  }
}
