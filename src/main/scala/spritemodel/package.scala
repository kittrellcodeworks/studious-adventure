import java.awt.image.BufferedImage
import java.awt.{Component, Graphics2D, Image, MediaTracker}
import java.io.{File, IOException}
import java.net.URL
import javax.imageio.ImageIO

package object spritemodel {


  @throws[IOException]
  def loadImage(imageName: String): BufferedImage = ImageIO.read(new File(imageName))

  @throws[IOException]
  def loadImage(imageUrl: URL): BufferedImage = ImageIO.read(imageUrl)

  private[spritemodel] def loadImages(image: Image *)(implicit c: Component): Unit = {
    val tracker = new MediaTracker(c)
    var i = 1
    image foreach { img ⇒
      tracker.addImage(img, i)
      i += 1
    }
    try
      tracker.waitForAll()
    catch {
      case _: InterruptedException ⇒
        println("error: couldn't load image, media tracker InterruptedException")
    }
  }

}
