package spritemodel

import java.net.URL
import java.util
import javax.imageio.ImageIO

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConversions._
import scala.util.Try

object Sprites {

  def load(spritesResourceName: String): Map[String, SpriteSheet] =
    parse(ConfigFactory.load(spritesResourceName))

  def load(spritesURL: URL): Map[String, SpriteSheet] =
    parse(ConfigFactory.parseURL(spritesURL))

  object TileDimension {
    def unapply(o: Any): Option[Int] = o match {
      case i: Int if i > 0 ⇒ Some(i)
      case _ ⇒ Some(1)
    }
  }

  object SpriteSheetDef {
    def unapply(conf: (String, util.Map[String, Any])): Option[(String, URL, Int, Int)] = {
      val name = conf._1
      val m = mapAsScalaMap(conf._2)
      for {
        url ← m.get("url").flatMap(s ⇒ Try(new URL(s.toString)).fold(
          _ ⇒ {System.err.println(s"Sprites:parse: bad url for sprite sheet '$name'"); None},
          Some(_)
        ))
        TileDimension(tilesAcross) ← m.get("x")
        TileDimension(tilesDown) ← m.get("y")
      } yield (name, url, tilesAcross, tilesDown)
    }
  }

  private def parse(spritesConfig: Config): Map[String, SpriteSheet] = (for {
    SpriteSheetDef(name, imgUrl, tilesAcross, tilesDown) <- mapAsScalaMap(spritesConfig.root.unwrapped)
    img ← Try(ImageIO.read(imgUrl)).fold(
      e ⇒ {System.err.println(s"Sprites:parse: error reading image for sprite sheet '$name': ${e.getMessage}"); None},
      Some(_)
    )
  } yield name → SpriteSheet(name, img, tilesAcross, tilesDown)).toMap

}
