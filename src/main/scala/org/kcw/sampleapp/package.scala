package org.kcw

import java.awt.{Color, Toolkit}
import java.awt.event.{MouseAdapter, MouseEvent}

import org.kcw.physics._
import org.kcw.sprite.{OutlineCircle, Rotation, Sprite, SpriteSheet}

package object sampleapp {

  val AppName = "Robowar"
  val MenuShortcutKeyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()

  // Save this for later
  class PopupMenuMouseAdapter(showPopup: MouseEvent ⇒ Unit) extends MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = evaluatePopup(e)
    override def mouseReleased(e: MouseEvent): Unit = evaluatePopup(e)
    private def evaluatePopup(e: MouseEvent): Unit = if (e.isPopupTrigger) showPopup(e)
  }

  class SpOb(override val spriteSheet: SpriteSheet, start: Point, r: Double) extends SpatialObject with Sprite {
    var shape: Shape = Circle(start, r)
    //override val outline: Option[OutlineCircle] = Some(OutlineCircle(Color.BLACK, r.toInt))
    override def x: Int = shape.asInstanceOf[Circle].origin.x.toInt
    override def y: Int = shape.asInstanceOf[Circle].origin.y.toInt
  }

  class SpObR(spriteSheet: SpriteSheet, start: Point, r: Double, rot: Int) extends SpOb(spriteSheet, start, r) with Rotation {
    var rotation: Int = rot
    override val outline: Option[OutlineCircle] = Some(OutlineCircle(Color.BLACK, r.toInt))
    override def x: Int = shape.asInstanceOf[Circle].origin.x.toInt
    override def y: Int = shape.asInstanceOf[Circle].origin.y.toInt
  }

}
