package org.kcw.physics

import scala.language.postfixOps

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

  def apply(a: BoundingBox, b: Ray): Boolean = CohenSutherland.intersects(b, a)

  def apply(a: Circle, b: Circle): Boolean = {
    val dr = a.radius + b.radius
    (a.origin distanceTo b.origin magnitudeSquared) < dr * dr
  }

  def apply(circ: Circle, ray: Ray): Boolean = {
    val f = circ.origin distanceTo ray.origin
    val r2 = circ.radius * circ.radius
    val containsRayStart = f.magnitudeSquared < r2
    containsRayStart || {
      val d = ray.direction

      val a = d dot d
      val b = 2 * (f dot d)
      val c = (f dot f) - r2

      val discriminant = b * b - 4 * a * c
      (discriminant >= 0) && { // ray didn't totally miss sphere,
        // so there is a solution to
        // the equation.
        val discriminantRoot = math.sqrt(discriminant)
        // either solution may be on or off the ray so need to test both
        // t1 is always the smaller value, because BOTH discriminant and
        // a are nonnegative.
        val t1 = (-b - discriminantRoot) / (2 * a)
        val t2 = (-b + discriminantRoot) / (2 * a)
        // 3x HIT cases:
        //          -o->             --|-->  |            |  --|->
        // Impale(t1 hit,t2 hit), Poke(t1 hit,t2>1), ExitWound(t1<0, t2 hit),
        // 3x MISS cases:
        //       ->  o                     o ->              | -> |
        // FallShort (t1>1,t2>1), Past (t1<0,t2<0), CompletelyInside(t1<0, t2>1)

        // t1 is the intersection, and it's closer than t2 (since t1 uses -b - discriminant): Impale, Poke
        (t1 >= 0 && t1 <= 1) ||
          // here t1 didn't intersect so we are either started inside the sphere or completely past it: ExitWound
          (t2 >= 0 && t2 <= 1)
        // no intn: FallShort, Past, but not CompletelyInside (that's caught above with contains)
      }
    }
  }

  def apply(a: Ray, b: Ray): Boolean = {
    val E = a.origin.distanceTo(b.origin)
    val F = b.direction
    val P = a.direction.rotate90ccw
    val d = F dot P
    if (d == 0) a.contains(b.origin) || a.contains(b.origin + b.direction)
    else {
      val h = (E dot P) / d
      h >= 0.0 && h <= 1.0
    }
  }

}
