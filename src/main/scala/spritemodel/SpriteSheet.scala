package spritemodel

import java.awt.Graphics2D
import java.awt.image.BufferedImage

case class SpriteSheet(name: String, tileWidth: Int, tileHeight: Int, tiles: IndexedSeq[BufferedImage]) {
  def getFace(index: Int): BufferedImage = tiles(index)

  def size: Int = tiles.size

  def paint(g: Graphics2D, tile: Int, x: Int, y: Int): Unit = if (tiles.isDefinedAt(tile)) {
    // TODO -- support scaling?
    g.drawImage(tiles(tile), x - tileWidth / 2, y - tileHeight / 2, tileWidth, tileHeight, null)
  }
}

object SpriteSheet {
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
