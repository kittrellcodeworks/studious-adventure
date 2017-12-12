package org.kcw

import scala.annotation.tailrec

package object physics {
  private[physics] val tolerance: Double = 0.00000001D

  private[physics] object CohenSutherland {
    type OutCode = Int

    val Inside: OutCode = 0; // 0000
    val Left: OutCode = 1;   // 0001
    val Right: OutCode = 2;  // 0010
    val Bottom: OutCode = 4; // 0100
    val Top: OutCode = 8;    // 1000

    def computeOutCode(x: Double, y: Double, bb: BoundingBox): OutCode =
      (if (x < bb.min.x) Left else if (x > bb.max.x) Right else Inside) |
        (if (y < bb.min.y) Bottom else if (y > bb.max.y) Top else Inside)

    def intersects(r: Ray, bb: BoundingBox): Boolean = {
      var x0 = r.origin.x
      var y0 = r.origin.y
      var x1 = x0 + r.direction.x
      var y1 = y0 + r.direction.y

      @inline
      def next(oc: OutCode): (Double, Double) = {
        if ((oc & Top) != 0) { // point is above the clip rectangle
          (x0 + (x1 - x0) * (bb.max.y - y0) / (y1 - y0), bb.max.y)
        }
        else if ((oc & Bottom) != 0) { // point is below the clip rectangle
          (x0 + (x1 - x0) * (bb.min.y - y0) / (y1 - y0), bb.min.y)
        }
        else if ((oc & Right) != 0) { // point is to the right of clip rectangle
          (bb.max.x, y0 + (y1 - y0) * (bb.max.x - x0) / (x1 - x0))
        }
        else { // if ((oc & Left) != 0) { // point is to the left of clip rectangle
          (bb.min.x, y0 + (y1 - y0) * (bb.min.x - x0) / (x1 - x0))
        }
      }

      @tailrec
      def inner(oc0: OutCode, oc1: OutCode): Boolean = {
        if ((oc0 | oc1) == Inside) true // Trivially accept both endpoints inside
        else if ((oc0 & oc1) != 0) false // implies both end points are in the same region outside the window. Reject and get out of loop
        else if (oc0 != Inside) {
          val (x, y) = next(oc0)
          x0 = x; y0 = y
          inner(computeOutCode(x0, y0, bb), oc1)
        } else {
          val (x, y) = next(oc1)
          x1 = x; y1 = y
          inner(oc0, computeOutCode(x1, y1, bb))
        }
      }

      inner(computeOutCode(x0, y0, bb), computeOutCode(x1, y1, bb))

      // Technically, we've computed a line-segment that is inside the bounding box. we can add a func to return that
      // if we need it, later.
    }
  }

}
