package org.kcw
package physics

import scala.language.postfixOps

sealed trait Shape {
  type Self
  def +(p: Vect): Self
  def -(p: Vect): Self

  def intersects(other: Shape): Boolean
  def contains(point: Point): Boolean
  val bounds: BoundingBox
}

case class Point(x: Double, y: Double) extends Shape with sprite.Point[Double] {
  type Self = Point

  override def +(v: Vect): Point = Point(x + v.x, y + v.y)

  override def -(v: Vect): Point = Point(x - v.x, y - v.y)

  override def intersects(other: Shape): Boolean = other contains this

  override def contains(point: Point): Boolean =
    math.abs(x - point.x) < tolerance && math.abs(y - point.y) < tolerance

  override lazy val bounds: BoundingBox = BoundingBox(this, this)

  def distanceTo(point: Point): Vect = Vect(point.x - x, point.y - y)

  def rangeTo(point: Point): Double = distanceTo(point).magnitude // expensive
  def rangeSquaredTo(point: Point): Double = distanceTo(point).magnitudeSquared // cheap

  override def equals(obj: Any): Boolean = obj match {
    case p: Point ⇒ contains(p)
    case _ ⇒ false
  }

  override def toString: String = "(" + x + "," + y + ")"
}

object Point {
  val zero = Point(0, 0)
}

case class BoundingBox(min: Point, max: Point) extends Shape {
  type Self = BoundingBox

  require(min.x <= max.x && min.y <= max.y, "Min coordinates must be less than max coordinates")

  lazy val center = Point((max.x + min.x) / 2, (max.y + min.y) / 2)

  override def +(v: Vect): BoundingBox = BoundingBox(min + v, max + v)

  override def -(v: Vect): BoundingBox = BoundingBox(min - v, max - v)

  override def intersects(other: Shape): Boolean = other match {
    case p: Point ⇒ contains(p)
    case b: BoundingBox ⇒ Collider(this, b)
    case c: Circle ⇒ Collider(this, c)
    case r: Ray ⇒ Collider(this, r)
  }

  override def contains(point: Point): Boolean =
    max.y + tolerance > point.y && max.x + tolerance > point.x && min.y < point.y + tolerance && min.x < point.x + tolerance

  override val bounds: BoundingBox = this

  def & (other: BoundingBox): Option[BoundingBox] = {
    if (Collider(this, other))
      Some(BoundingBox(Point(math.max(min.x, other.min.x), math.max(min.y, other.min.y)),
        Point(math.min(max.x, other.max.x), math.min(max.y, other.max.y))))
    else None
  }
}

case class Circle(origin: Point, radius: Double) extends Shape {
  type Self = Circle

  override def +(v: Vect): Circle = copy(origin + v)

  override def -(v: Vect): Circle = copy(origin - v)

  def intersects(other: Shape) = other match {
    case p: Point ⇒ contains(p)
    case b: BoundingBox ⇒ Collider(b, this)
    case c: Circle ⇒ Collider(this, c)
    case r: Ray ⇒ Collider(this, r)
  }

  override def contains(point: Point): Boolean =
    (origin distanceTo point magnitudeSquared) < radius * radius

  override lazy val bounds: BoundingBox =
    BoundingBox(Point(origin.x - radius, origin.y - radius), Point(origin.x + radius, origin.y + radius))

  override def equals(obj: Any): Boolean = obj match {
    case Circle(o, r) ⇒ origin == o && math.abs(radius - r) < tolerance
    case _ ⇒ false
  }
}

case class Ray(origin: Point, direction: Vect) extends Shape {
  type Self = Ray

  override def +(p: Vect): Ray = Ray(origin + p, direction)

  override def -(p: Vect): Ray = Ray(origin - p, direction)

  override def intersects(other: Shape): Boolean = other match {
    case p: Point ⇒ contains(p)
    case b: BoundingBox ⇒ Collider(b, this)
    case c: Circle ⇒ Collider(c, this)
    case r: Ray ⇒ Collider(this, r)
  }

  override def contains(point: Point): Boolean = {
    val dist = origin distanceTo point
    dist == (dist project direction)
  }

  override lazy val bounds: BoundingBox = {
    val outerx = origin.x + direction.x
    val outery = origin.y + direction.y

    if (direction.x > 0) {
      if (direction.y > 0) BoundingBox(origin, Point(outerx, outery))
      else BoundingBox(Point(origin.x, outery), Point(outerx, origin.y))
    }
    else {
      if (direction.y > 0) BoundingBox(Point(outerx, origin.y), Point(origin.x, outery))
      else BoundingBox(Point(outerx, outery), origin)
    }
  }
}
