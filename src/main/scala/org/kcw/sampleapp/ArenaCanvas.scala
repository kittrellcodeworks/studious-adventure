package org.kcw
package sampleapp

import java.awt.{Point ⇒ _, _}
import java.awt.event._

import org.kcw.physics.SpatialMap.SpatialMapContext
import org.kcw.physics._
import org.kcw.sampleapp.ArenaCanvas._


object ArenaCanvas {
  val spritesheets: Map[String, sprite.SpriteSheet] = sprite.Sprites.load("sprites")
}

class ArenaCanvas extends Canvas with sprite.World {

  // TODO - split SpOb World into separate class.

  private var _origin = physics.Point.zero
  private var _zoom = 1D

  override def origin: sprite.Point[Double] = _origin
  def zoom: Double = _zoom

  def displayableEffects: Iterable[sprite.GraphicEffect] = {
    val d = this.getSize
    val viewMax = _origin + (Vect(d.width, d.height) / _zoom)
    val viewBounds = BoundingBox(_origin, viewMax)
    val objs = spatialMap.objectsNear(viewBounds)
    val paintable = objs collect { case Paintable(obj) ⇒ obj }
    paintable
  }

  val o1 = new SpOb(spritesheets("asteroid1"), Point(175.5, 30.5), 27)
  val o2 = new SpOb(spritesheets("asteroid2"), Point(20, 87), 20)
  val o3 = new SpOb(spritesheets("asteroid3"), Point(280, 195), 13)

  private var spatialMap: SpatialMap[SpOb] = SpatialMap(
    SpatialMapContext(300, 300, 60), o1, o2, o3)

  setMinimumSize(new Dimension(300, 300))
  setPreferredSize(new Dimension(300, 300))

  private val mouseAdapter = new MouseAdapter {
    var pressedPoint: Option[(Int, Int)] = None

    val dragOnMask = InputEvent.BUTTON1_DOWN_MASK
    val dragOffMask = InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK |
      InputEvent.ALT_DOWN_MASK

    override def mousePressed(e: MouseEvent): Unit = {
      if ((e.getModifiersEx & (dragOnMask | dragOffMask)) == dragOnMask) {
        pressedPoint = Some((e.getX, e.getY))
      }
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      pressedPoint foreach { p ⇒
        val x = e.getX
        val y = e.getY
        translateTo(_origin - (Vect(x - p._1, y - p._2) / zoom), zoom)
        pressedPoint = Some((x, y))
      }
    }

    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      val notches: Int = e.getWheelRotation
      var temp: Double = zoom - (notches * 0.2)
      // minimum zoom factor is 1.0
      temp = math.min(math.max(temp, 0.2), 2.0)
      if (temp != zoom) translateTo(_origin, temp)
    }
  }

  addMouseListener(mouseAdapter)
  addMouseWheelListener(mouseAdapter)
  addMouseMotionListener(mouseAdapter)

  override def paint(g: Graphics): Unit = try {
    super[Canvas].paint(g)
    val g2d = g.asInstanceOf[Graphics2D]

    val d = this.getSize
    g.setColor(Color.LIGHT_GRAY)
    g.fillRect(0,0,d.width, d.height)

    val viewMax = _origin + (Vect(d.width, d.height) / zoom)
    val viewBounds = BoundingBox(_origin, viewMax)
    val spaceBounds = spatialMap.ctx.bounds

    val offsetVect = _origin distanceTo spaceBounds.min

    //outline the world
    g.setColor(Color.GRAY)
    val bx = (offsetVect.x * _zoom).toInt
    val by = (offsetVect.y * _zoom).toInt
    val bw = ((spaceBounds.max.x - spaceBounds.min.x) * _zoom).toInt
    val bh = ((spaceBounds.max.y - spaceBounds.min.y) * _zoom).toInt
    g.clearRect(bx, by, bw, bh)
    g.drawRect(bx, by, bw, bh)
    g.setClip(bx, by, bw, bh)

    // prep the graphics for printing the visible objects
    paintEffect(g2d)
  } catch {
    case t: Throwable ⇒ System.err.println(t.getStackTraceString)
  }

  def setContents(sm: SpatialMap[SpOb]): Unit = {
    spatialMap = sm
    repaint
  }

  def translateTo(newOrigin: Point, newZoom: Double) = {
    _origin = newOrigin
    _zoom = newZoom
    repaint
  }

}
