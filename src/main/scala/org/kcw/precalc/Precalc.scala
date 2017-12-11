package org.kcw

package object precalc {

  // for simplicity's sake, we'll use precalculated values for integral degrees.
  private val cosineTable: Seq[Double] = for (i ← Range(0, 360)) yield i match {
    // special cases for 90-degree angles to avoid issues with precision bits in floating point math
    case 0 ⇒ 1.0D
    case 90 ⇒ 0.0D
    case 180 ⇒ -1.0D
    case 270 ⇒ 0.0D
    case i ⇒ math.cos(i * math.Pi / 180.0)
  }
  private val sineTable: Seq[Double] = cosineTable.drop(270) ++ cosineTable.take(270)
  private val tanTable: Seq[Double] = for (i ← Range(0, 180)) yield {
    if (cosineTable(i) == 0.0) Double.PositiveInfinity else sineTable(i) / cosineTable(i)
  }

  def cos(deg: Int): Double = cosineTable(if (deg < 0) (deg % 360) + 360 else deg % 360)
  def sin(deg: Int): Double = sineTable(if (deg < 0) (deg % 360) + 360 else deg % 360)
  def tan(deg: Int): Double = tanTable(if (deg < 0) (deg % 180) + 180 else deg % 180)

  def unsafe_cos(deg: Int): Double = cosineTable(deg)
  def unsafe_sin(deg: Int): Double = sineTable(deg)
  def unsafe_tan(deg: Int): Double = tanTable(deg)

}
