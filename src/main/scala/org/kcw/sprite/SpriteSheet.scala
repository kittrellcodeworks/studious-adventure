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
    g.drawImage(tiles(tile), x - halfWidth, y - halfHeight, tileWidth, tileHeight, null)
  }
}

object SpriteSheet {
  val empty: SpriteSheet = new SpriteSheet("", 0, 0, IndexedSeq.empty) {
    override def paint(g: Graphics2D, tile: Int, x: Int, y: Int): Unit = { }
  }

  def apply(name: String, srcImage: BufferedImage, tilesAcross: Int, tilesHigh: Int): SpriteSheet = {
    val tileWidth = srcImage.getWidth / tilesAcross
    val tileHeight = srcImage.getHeight / tilesHigh

    val tiles = for {
      k ← 0 to tilesHigh
      m ← 0 to tilesAcross
    } yield srcImage.getSubimage(m * tileWidth, k * tileHeight, tileWidth, tileHeight)

    SpriteSheet(name, tileWidth, tileHeight, tiles)
  }
}
