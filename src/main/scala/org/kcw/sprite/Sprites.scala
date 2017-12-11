package org.kcw.sprite

import java.net.URL
import java.util
import javax.imageio.ImageIO

import com.typesafe.config.{Config, ConfigFactory, ConfigValue, ConfigValueType}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Try

object Sprites {

  def load(spritesResourceName: String): Map[String, SpriteSheet] =
    parse(ConfigFactory.load(spritesResourceName))

  def load(spritesURL: URL): Map[String, SpriteSheet] =
    parse(ConfigFactory.parseURL(spritesURL))

  private object TileDimension {
    def unapply(o: Any): Option[Int] = o match {
      case i: Int if i > 0 ⇒ Some(i)
      case _ ⇒ Some(1)
    }
  }

  private object SpriteSheetDef {
    def unapply(conf: util.Map.Entry[String, AnyRef]): Option[(String, URL, Dimension[Int], Int, Dimension[Int], Point[Int], Dimension[Int])] = {
      val name = conf.getKey
      conf.getValue match {
        case v: util.Map[_, _] ⇒
          val m = v.asInstanceOf[util.Map[String, Any]].asScala
          for {
            url ← m.get("url").flatMap { s ⇒
              val ss = s.toString
              if (ss.contains(":")) {
                Try(new URL(ss)).map(Option(_)).recover {
                  case e ⇒ System.err.println(s"Sprites:parse: bad url ($ss) for sprite sheet '$name'")
                    e.printStackTrace
                    None
                }.get
              }
              else {
                Option(getClass.getResource("/"+ss))
              }
            }
            tilesAcross = m.get("tiles-across").collect { case i: Int if i > 0 ⇒ i } getOrElse 1
            tilesHigh = m.get("tiles-high").collect { case i: Int if i > 0 ⇒ i } getOrElse 1
            max = tilesAcross * tilesHigh
            count = m.get("tile-count").collect { case i: Int if i > 0 && i < max ⇒ i } getOrElse max
            sizeX = m.get("width").collect { case i: Int if i > 0 ⇒ i } getOrElse 0
            sizeY = m.get("height").collect { case i: Int if i > 0 ⇒ i } getOrElse 0
            offsetX = m.get("offset-x").collect { case i: Int if i > 0 ⇒ i } getOrElse 0
            offsetY = m.get("offset-y").collect { case i: Int if i > 0 ⇒ i } getOrElse 0
            spacingX = m.get("spacing-x").collect { case i: Int if i > 0 ⇒ i } getOrElse 0
            spacingY = m.get("spacing-y").collect { case i: Int if i > 0 ⇒ i } getOrElse 0
          } yield (name, url, IDim(tilesAcross, tilesHigh), count, IDim(sizeX, sizeY), IPoint(offsetX, offsetY), IDim(spacingX, spacingY))
        case _ ⇒ None
      }
    }
  }

  private def parse(spritesConfig: Config): Map[String, SpriteSheet] = {
    val conf = spritesConfig.root.unwrapped.entrySet.asScala.toSeq
    val x = for {
      SpriteSheetDef(name, imgUrl, tileCount, count, imgSize, offset, spacing) <- conf
      img ← Try(ImageIO.read(imgUrl)).map(Some(_)).recover {
        case e ⇒
          System.err.println(s"Sprites:parse: error reading image for sprite sheet '$name': ${e.getMessage}")
          e.printStackTrace
          None
      }.get
    } yield name → SpriteSheet(name, img, tileCount, count, imgSize, offset, spacing)
    x.toMap
  }

}
