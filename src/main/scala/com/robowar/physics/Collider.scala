package com.robowar.physics

object Collider {

  def apply(a: BoundingBox, b: BoundingBox): Boolean =
    a.min.x < b.max.x + tolerance && a.max.x + tolerance > b.min.x &&
      a.min.y < b.max.y + tolerance && a.max.y + tolerance > b.min.y

  def apply(a: BoundingBox, b: Circle): Boolean = {
    val d = a.center distanceTo b.origin
    val dm2 = d.magnitudeSquared

    // project vectors to each corner onto the separation vector (d)
    // NOTE: This algorithm is more suited to Rects that can be rotated. Save for later.
    //val cornerVects = Seq(
    //  a.center distanceTo a.min,
    //  a.center distanceTo a.max,
    //  Vect(a.max.x - a.center.x, a.min.y - a.center.y),
    //  Vect(a.min.x - a.center.x, a.max.y - a.center.y)
    //)
    // val dot = cornerVects.map(_ dot d).reduceLeft(math.max)

    val closestCornerVect = Vect(
      (if (b.origin.x > a.center.x) a.max.x else a.min.x) - a.center.x,
      (if (b.origin.y > a.center.y) a.max.y else a.min.y) - a.center.y
    )

    b.radius * math.sqrt(dm2) + (closestCornerVect dot d) > dm2
  }

  def apply(a: Circle, b: Circle): Boolean = {
    val dr = a.radius + b.radius
    (a.origin distanceTo b.origin magnitudeSquared) < dr * dr
  }

}
