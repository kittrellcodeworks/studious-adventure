package com.robowar.physics

/**
  * Vects (Vectors, really, but we don't want to clash with the scala collection name) can represent Velocity,
  * Accelleration, or any other relationship between points.
  *
  * @param x rise
  * @param y run
  */
case class Vect(x: Double, y: Double) {
  // def slope = x / y // also tangent(theta)

  def +(a: Vect): Vect = Vect(x + a.x, y + a.y)
  def -(a: Vect): Vect = Vect(x - a.x, y - a.y)

  def *(m: Double): Vect = Vect(x * m, y * m)
  def /(d: Double): Vect = Vect(x / d, y / d)

  def dot(other: Vect): Double = (x * other.x) + (y * other.y)
  def magnitudeSquared: Double = (x * x) + (y * y)
  def magnitude: Double = math.sqrt(magnitudeSquared)

  // def unit: Vector = this / magnitude

  def project(onto: Vect): Vect =
  // standard formula is onto.unit * (this dot onto.unit); but we'll simplify to avoid calling sqrt
    onto * (this dot onto) / onto.magnitudeSquared

  def projectedMagnitude(onto: Vect): Double = (this dot onto) / onto.magnitude

  def rotate90cw: Vect = Vect(y, -x)
  def rotate90ccw: Vect = Vect(-y, x)

  override def equals(obj: Any): Boolean = obj match {
    case Vect(x2, y2) ⇒ math.abs(x - x2) < tolerance && math.abs(y - y2) < tolerance
    case _ ⇒ false
  }

  override def toString: String = "<" + x + "," + y + ">"
}

object Vect {
  val zero = Vect(0, 0)
}
