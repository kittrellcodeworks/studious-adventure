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
    def unapply(conf: util.Map.Entry[String, AnyRef]): Option[(String, URL, Int, Int, Int)] = {
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
            TileDimension(tilesAcross) ← m.get("x")
            TileDimension(tilesDown) ← m.get("y")
            max = tilesAcross * tilesDown
            count = m.get("count").collect { case i: Int if i > 0 && i < tilesAcross * tilesDown ⇒ i } getOrElse max
          } yield (name, url, tilesAcross, tilesDown, count)
        case _ ⇒ None
      }
    }
  }

  private def parse(spritesConfig: Config): Map[String, SpriteSheet] = {
    val conf = spritesConfig.root.unwrapped.entrySet.asScala.toSeq
    val x = for {
      SpriteSheetDef(name, imgUrl, tilesAcross, tilesDown, count) <- conf
      img ← Try(ImageIO.read(imgUrl)).map(Some(_)).recover {
        case e ⇒
          System.err.println(s"Sprites:parse: error reading image for sprite sheet '$name': ${e.getMessage}")
          e.printStackTrace
          None
      }.get
    } yield name → SpriteSheet(name, img, tilesAcross, tilesDown, count)
    x.toMap
  }

}
