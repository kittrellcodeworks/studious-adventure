package org.kcw.physics

import java.awt.Graphics2D

import scala.language.implicitConversions

trait SpatialObject {
  def shape: Shape // for specific collision phase

  final def intersects(other: SpatialObject): Boolean =
    (shape.bounds intersects other.shape.bounds) && (shape intersects other.shape)
}

trait Paintable { self: SpatialObject ⇒
  def paint(g: Graphics2D): Unit
}

object Paintable {
  def unapply(d: SpatialObject with Paintable): Option[SpatialObject with Paintable] = Some(d)
}

object SpatialObject {
  implicit def fromShape(s: Shape): SpatialObject = new GenericSpatialObject(s)
}

class GenericSpatialObject(val shape: Shape) extends SpatialObject with Paintable {
  override def toString: String = shape.toString

  override def paint(g: Graphics2D): Unit = shape match {
    case Point(x, y) ⇒
      g.drawOval(x.toInt, y.toInt, 1, 1)

    case BoundingBox(Point(minX, minY), Point(maxX, maxY)) ⇒
      val w = math.max(math.round(maxX - minX).toInt, 1)
      val h = math.max(math.round(maxY - minY).toInt, 1)
      g.drawRect(minX.toInt, minY.toInt, w, h)

    case Circle(Point(x, y), radius) ⇒
      val r = radius.toInt
      g.drawOval(x.toInt - r, y.toInt - r, 2 * r, 2 * r)
  }
}
