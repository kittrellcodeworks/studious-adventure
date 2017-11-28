package com.robowar.application

import java.awt.event._
import java.awt.{Point ⇒ AwtPoint, _}

import com.robowar.actors.Debouncer
import com.robowar.physics._
import com.robowar.physics.SpatialMap.SpatialMapContext

import akka.actor.{Actor, ActorRefFactory}
import scala.concurrent.duration._

object ArenaCanvas {
}

class ArenaCanvas extends Canvas {

  private var origin: Point = Point.zero
  private var zoom: Double = 1

  private var spatialMap: SpatialMap = SpatialMap(
    SpatialMapContext(300, 300, 60),
    Circle(Point(175.5, 30.5), 16),
    Circle(Point(20, 87), 16),
    Circle(Point(280, 195), 16)
  )

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
        translateTo(origin - (Vect(x - p._1, y - p._2) / zoom), zoom)
        pressedPoint = Some((x, y))
      }
    }

    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      val notches: Int = e.getWheelRotation
      var temp: Double = zoom - (notches * 0.2)
      // minimum zoom factor is 1.0
      temp = math.min(math.max(temp, 0.2), 2.0)
      if (temp != zoom) translateTo(origin, temp)
    }
  }

  addMouseListener(mouseAdapter)
  addMouseWheelListener(mouseAdapter)
  addMouseMotionListener(mouseAdapter)

  // limit frames per second to about 60.
  //val debouncer = actorContext.actorOf(Debouncer.props[SpatialMap, SpatialMap](16.66.millis, null, (_, m) ⇒ m, receiveMap))

  override def paint(g: Graphics): Unit = try {
    super.paint(g)
    val g2d = g.asInstanceOf[Graphics2D]

    val d = this.getSize
    g.setColor(Color.LIGHT_GRAY)
    g.fillRect(0,0,d.width, d.height)

    val viewMax = origin + (Vect(d.width, d.height) / zoom)
    val viewBounds = BoundingBox(origin, viewMax)
    val spaceBounds = spatialMap.ctx.bounds


    val offsetVect = origin distanceTo spaceBounds.min

    g.setColor(Color.GRAY)
    val bx = (offsetVect.x * zoom).toInt
    val by = (offsetVect.y * zoom).toInt
    val bw = ((spaceBounds.max.x - spaceBounds.min.x) * zoom).toInt
    val bh = ((spaceBounds.max.y - spaceBounds.min.y) * zoom).toInt
    g.clearRect(bx, by, bw, bh)
    g.drawRect(bx, by, bw, bh)
    g.setClip(bx, by, bw, bh)

    g2d.setColor(Color.BLACK)

    val objs = spatialMap.objectsNear(viewBounds)
    for (Drawable(obj) ← objs) obj.draw(origin, zoom, g2d)

  } catch {
    case t: Throwable ⇒ System.err.println(t.getStackTraceString)
  }

  def setContents(sm: SpatialMap): Unit = {
    spatialMap = sm
    repaint
  }

  def translateTo(newOrigin: Point, newZoom: Double) = {
    origin = newOrigin
    zoom = newZoom
    repaint
  }

}
