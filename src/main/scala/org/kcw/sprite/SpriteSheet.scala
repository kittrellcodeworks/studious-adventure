package org.kcw.sprite

import java.awt.Graphics2D
import java.awt.image.BufferedImage

case class SpriteSheet(name: String, tileWidth: Int, tileHeight: Int, tiles: IndexedSeq[BufferedImage]) {
  private val halfWidth = tileWidth / 2
  private val halfHeight = tileHeight / 2

  def isEmpty: Boolean = tiles.isEmpty

  def nonEmpty: Boolean = tiles.nonEmpty

  def size: Int = tiles.size

  def paint(g: Graphics2D, tile: Int, x: Int, y: Int): Unit = if (tiles.isDefinedAt(tile)) {
    g.drawImage(tiles(tile), x - halfWidth, y - halfHeight, null)
  }
}

object SpriteSheet {
  val empty: SpriteSheet = new SpriteSheet("", 0, 0, IndexedSeq.empty) {
    override def paint(g: Graphics2D, tile: Int, x: Int, y: Int): Unit = { }
  }

  def apply(name: String, srcImage: BufferedImage, tilesAcross: Int, tilesHigh: Int, count: Int = Int.MaxValue): SpriteSheet = {
    val tileWidth: Int = srcImage.getWidth / tilesAcross
    val tileHeight: Int = srcImage.getHeight / tilesHigh
    val max: Int = math.min(count, tileWidth * tileHeight)

    val tiles = for {
      k ← 0 until tilesHigh
      m ← 0 until tilesAcross
      if k * tilesAcross + m < max
    } yield srcImage.getSubimage(m * tileWidth, k * tileHeight, tileWidth, tileHeight)

    SpriteSheet(name, tileWidth, tileHeight, tiles)
  }
}
