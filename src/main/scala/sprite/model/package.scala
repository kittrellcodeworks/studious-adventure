package sprite

import java.awt.image.{AffineTransformOp, BufferedImage}
import java.awt._
import java.io.{File, IOException}
import java.net.URL
import javax.imageio.ImageIO

package object model {

  case class Dimension[T](w: T, h: T)
  case class Rect[T](x: T, y: T, w: T, h: T)

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
