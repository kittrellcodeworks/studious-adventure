package com.robowar.physics

object Precalc {

  // for simplicity's sake, we'll use precalculated values for integral degrees.
  private val cosineTable: Seq[Double] = for {i ← Range(0, 360); r = i * math.Pi / 180.0} yield math.cos(r)
  private val sineTable: Seq[Double] = cosineTable.drop(270) ++ cosineTable.take(270)
  private val tanTable: Seq[Double] = for (i ← Range(0, 180)) yield {
    if (cosineTable(i) == 0.0) Double.PositiveInfinity else sineTable(i) / cosineTable(i)
  }

  def cos(deg: Int): Double = cosineTable(if (deg < 0) deg % 360 + 360 else deg % 360)
  def sin(deg: Int): Double = sineTable(if (deg < 0) deg % 360 + 360 else deg % 360)
  def tan(deg: Int): Double = tanTable(if (deg < 0) deg % 180 + 180 else deg % 180)

}
