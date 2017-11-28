package com.robowar.physics

import java.awt.Graphics2D

import scala.language.implicitConversions

trait SpatialObject {
  def shape: Shape // for specific collision phase

  final def intersects(other: SpatialObject): Boolean =
    (shape.bounds intersects other.shape.bounds) && (shape intersects other.shape)
}

trait Drawable { self: SpatialObject ⇒
  def draw(offset: Point, zoom: Double, g: Graphics2D): Unit
}

object Drawable {
  def unapply(d: SpatialObject with Drawable): Option[SpatialObject with Drawable] = Some(d)
}

object SpatialObject {
  implicit def fromShape(s: Shape): SpatialObject = new GenericSpatialObject(s)
}

class GenericSpatialObject(val shape: Shape) extends SpatialObject with Drawable {
  override def toString: String = shape.toString

  override def draw(offset: Point, zoom: Double, g: Graphics2D): Unit = shape match {
    case Point(x, y) ⇒
      val s = math.max(zoom.toInt, 1)
      g.drawOval(((x - offset.x) * zoom).toInt, ((y - offset.y) * zoom).toInt, s, s)
    case BoundingBox(Point(minX, minY), Point(maxX, maxY)) ⇒
      val w = math.max(((maxX - minX) * zoom).toInt, 1)
      val h = math.max(((maxY - minY) * zoom).toInt, 1)
      g.drawRect(((minX - offset.x) * zoom).toInt, ((minY - offset.y) * zoom).toInt, w, h)
    case Circle(Point(x, y), radius) ⇒
      val s = math.max((radius * 2 * zoom).toInt, 1)
      g.drawOval(((x - radius - offset.x) * zoom).toInt, ((y - radius - offset.y) * zoom).toInt, s, s)
  }
}
