package org.kcw.sprite

import java.awt.Graphics2D

trait World extends GraphicEffect {

  def origin: Point[Double]
  def zoom: Double
  def displayableEffects: Iterable[GraphicEffect]

  override def paint(g: Graphics2D): Unit = {
    val o = origin
    val z = zoom
    val tx = g.getTransform
    g.scale(z, z)
    g.translate(o.x, o.y)
    for (ge ← displayableEffects) ge.paint(g)
    g.setTransform(tx) // put the graphixs transform back the way it was.
  }

}
