package org.kcw

import java.awt.image.BufferedImage
import java.awt._
import java.io.{File, IOException}
import java.net.URL
import javax.imageio.ImageIO

package object sprite {

  trait Point[T] { def x: T; def y: T }
  trait Dimension[T] { def w: T; def h: T }
  trait Rect[T] extends Point[T] with Dimension[T]
  trait Circle[T] extends Point[T] { def r: T }

  case class IPoint(x: Int, y: Int) extends Point[Int]
  case class IDim(w: Int, h: Int) extends Dimension[Int]
  case class IRect(x: Int, y: Int, w: Int, h: Int) extends Rect[Int]

  case class OutlineCircle(color: Color, radius: Int) {
    def paint(g: Graphics2D, x: Int, y: Int): Unit = if (radius > 0) {
      g.setColor(color)
      val r2 = 2 * radius
      g.drawOval(x - radius, y - radius, r2, r2)
    }
  }

  @throws[IOException]
  def loadImage(imageName: String): BufferedImage = ImageIO.read(new File(imageName))

  @throws[IOException]
  def loadImage(imageUrl: URL): BufferedImage = ImageIO.read(imageUrl)

}
